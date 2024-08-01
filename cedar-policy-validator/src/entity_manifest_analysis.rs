use std::collections::HashMap;

use cedar_policy_core::ast::Var;
use smol_str::SmolStr;

use crate::entity_manifest::{AccessPath, AccessTrie, EntityRoot, RootAccessTrie};

/// An `AccessPathRecord` represents `AccessPath`s possibly
/// wrapped in record literals.
/// This allows the Entity Manifest to soundly handle
/// data that is wrapped in record literals, then dereferenced.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AccessPathRecord {
    AccessPath(AccessPath),
    RecordLiteral(HashMap<SmolStr, Box<AccessPathRecord>>),
}

/// During Entity Manifest analysis, each sub-expression
/// produces an [`EntityManifestAnalysisResult`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EntityManifestAnalysisResult {
    /// INVARIANT: The `global_trie` stores all of the data paths this sub-expression
    /// could have accessed, including all those in `resulting_paths`.
    pub(crate) global_trie: RootAccessTrie,
    /// `resulting_paths` stores a list of `AccessPathRecord`,
    /// Each representing a data path
    /// (possibly wrapped in a record literal)
    /// that could be accessed using the `.` operator.
    pub(crate) resulting_paths: Vec<AccessPathRecord>,
}

impl Default for EntityManifestAnalysisResult {
    fn default() -> Self {
        Self {
            global_trie: RootAccessTrie::default(),
            resulting_paths: Vec::new(),
        }
    }
}

impl EntityManifestAnalysisResult {
  /// Drop the resulting paths part of the analysis.
  /// This is sound when the expression is not followed by a
  /// field dereference or check (`.` or `has`).
  pub fn drop_value(mut self) -> Self {
    self.resulting_paths = Vec::new();
    self
  }

  /// Union two [`EntityManifestAnalysisResult`]s together,
  /// keeping the paths from both global tries and concatenating
  /// the resulting paths.
    pub fn union(mut self, other: &Self) -> Self {
        self.global_trie = self.global_trie.union(&other.global_trie);
        self.resulting_paths.extend(other.resulting_paths.clone());
        self
    }

    /// Create an analysis result that starts with a cedar variable
    pub fn from_root(root: EntityRoot) -> Self {
        let path = AccessPath {
            root,
            path: vec![],
            ancestors_required: false,
        };
        Self {
            global_trie: path.to_root_access_trie(),
            resulting_paths: vec![AccessPathRecord::AccessPath(path)],
        }
    }

    /// Extend all the access paths with this attr,
    /// adding all the new paths to the global trie.
    pub fn get_attr(mut self, attr: &SmolStr) -> Self {
        for path_or_record in self.resulting_paths.iter_mut() {
            match path_or_record {
                AccessPathRecord::AccessPath(access_path) => {
                    access_path.path.push(attr.clone());
                }
                AccessPathRecord::RecordLiteral(record) => {
                    let Some(field) = record.get(attr) else {
                        panic!("Record literal lacks dereferenced field, but the typechecker failed to catch it.");
                    };
                    // TODO is there a nice way to avoid this clone?
                    *path_or_record = (**field).clone()
                }
            }
        }
        self.restore_global_trie_invariant(&AccessTrie::new())
    }

    /// Restores the `global_trie` invariant by adding all paths
    /// in `resulting_paths` to the `global_trie`.
    /// This is necessary after modifying the `resulting_paths`.
    /// This is a separate function to make it easier to test.
    pub(crate) fn restore_global_trie_invariant(mut self, leaf_trie: &AccessTrie) -> Self {
        self.global_trie = RootAccessTrie::default();
        for path_or_record in self.resulting_paths.iter() {
            self.global_trie.add_access_path_record(path_or_record, leaf_trie);
        }
        self
    }
}

impl RootAccessTrie {
    pub(crate) fn add_access_path_record(&mut self, path: &AccessPathRecord, leaf_trie: &AccessTrie) {
        match path {
            AccessPathRecord::AccessPath(access_path) => {
                self.add_access_path(access_path, leaf_trie);
            }
            AccessPathRecord::RecordLiteral(record) => {
                for field in record.values() {
                    self.add_access_path_record(field, leaf_trie);
                }
            }
        }
    }

    pub(crate) fn add_access_path(&mut self, access_path: &AccessPath, leaf_trie: &AccessTrie) {
        // could be more efficient by mutating self
        // instead we use the existing union function.
        let other_trie = access_path.to_root_access_trie_with_leaf(leaf_trie.clone());
        *self = self.union(&other_trie);
    }
}

// TODO move analysis code here
