use std::fs;
use std::path::PathBuf;

pub struct FileSet {
    pub files: Vec<PathBuf>,
}

impl FileSet {
    pub fn new() -> Self {
        FileSet { files: Vec::new() }
    }

    pub fn from_file(path: PathBuf) -> Self {
        FileSet { files: vec![path] }
    }

    pub fn from_directory(path: &PathBuf) -> Result<Self, std::io::Error> {
        let mut file_set = FileSet::new();
        file_set.collect_sea_files(path)?;
        Ok(file_set)
    }

    fn collect_sea_files(&mut self, dir: &PathBuf) -> Result<(), std::io::Error> {
        let entries = fs::read_dir(dir)?;

        for entry in entries {
            let entry = entry?;
            let path = entry.path();

            if !path.is_file() {
                continue;
            }
            let Some(ext) = path.extension() else {
                continue;
            };
            if ext != "sea" {
                continue;
            }
            self.files.push(path);
        }

        Ok(())
    }
}
