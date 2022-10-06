/// a path, such as `std::vec::Vec::push`. path components are literally separated by the pattern
/// "::".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    components: Vec<String>,
}

pub struct Components<'a> {
    path: &'a str,
}

impl<'a> Iterator for Components<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.path.split_once("::");
        match t {
            Some((res, path)) => {
                self.path = path;
                Some(res)
            }
            None => Some(self.path),
        }
    }
}

impl Path {
    pub fn new(components: Vec<String>) -> Self {
        Self { components }
    }

    pub fn components(&self) -> impl Iterator<Item = &String> {
        self.components.iter()
    }
}

impl<T> From<T> for Path
where
    String: From<T>,
{
    fn from(t: T) -> Self {
        Self {
            components: String::from(t)
                .split("::")
                .map(|s| s.trim().to_owned())
                .collect(),
        }
    }
}
