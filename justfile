mod rpkg

[default]
_default:
  @just --list --list-submodules

# Run clippy
clippy: && rpkg::clippy
  cargo clippy

# Run rust test
test: clippy && rpkg::test
  cargo test
