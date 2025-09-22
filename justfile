r_source := 'R/*'
build_path := 'build'

# Default to just --list
default:
  @just --list

# Build the package source
build: document
    mkdir -p {{build_path}}
    R -e 'devtools::build(path = "{{build_path}}")'
    R -e 'devtools::build_manual("{{build_path}}", "{{build_path}}")'

# Generate documentation
document:
  R -e 'devtools::document()'

# Check a build
check: build
    R -e 'devtools::check_built("{{build_path}}", cran = TRUE)'

# Performs sanity check on code
sanity-check: build
	R -e 'devtools::spell_check("{{build_path}}")'
