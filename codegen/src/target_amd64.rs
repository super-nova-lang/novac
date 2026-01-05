use anyhow::{bail, Result};
use parser::nodes::Node;

/// Placeholder AMD64 backend entrypoint. Returns an error until implemented.
pub fn gen_target(_module_name: &str, _ast: &[Node]) -> Result<String> {
	bail!("amd64 backend not implemented yet")
}

