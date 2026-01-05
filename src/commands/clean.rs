use anyhow::Result;
use std::path::Path;

pub fn run() -> Result<()> {
    let targets = ["build/emit", "build/debug"];
    for t in &targets {
        let p = Path::new(t);
        if p.exists() {
            for entry in std::fs::read_dir(p)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    std::fs::remove_dir_all(&path)?;
                } else {
                    std::fs::remove_file(&path)?;
                }
            }
        }
    }
    println!("Cleaned build/emit and build/debug");
    Ok(())
}
