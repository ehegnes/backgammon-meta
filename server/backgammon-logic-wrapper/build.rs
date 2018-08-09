extern crate cbindgen;

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Quit if not being run from `cargo`
    if !env::var("CARGO").unwrap().ends_with("cargo") {
        return;
    }

    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let config = cbindgen::Config::from_root_or_default(PathBuf::from(&crate_dir).as_path());

    fs::create_dir_all("include").expect("Could not create `include` directory.");

    cbindgen::Builder::new()
      .with_crate(crate_dir.clone())
      .with_config(config)
      .generate()
      .unwrap()
      .write_to_file("include/backgammonlogic.h");
}
