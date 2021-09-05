#[derive(Clone, Debug)]
struct DFS {
    n: usize,
    adj: Vec<Vec<usize>>,
    res: Vec<usize>,
    visited: Vec<bool>,
}

impl DFS {
    fn new(n: usize, adj: Vec<Vec<usize>>) -> Self {
        Self {
            n,
            adj,
            res: vec![],
            visited: vec![false;n],
        }
    }

    fn dfs(&mut self, u: usize) {
        self.visited[u] = true;
        for v in self.adj[u].clone() {
            if !self.visited[v] {
                self.dfs(v);
            }
        }
        self.res.push(u);
    }
}

pub fn do_topological_sort(adj: Vec<Vec<usize>>) -> Vec<usize> {
    assert_eq!(adj.len() > 0, true);
    let mut dfs = DFS::new(adj.len(), adj);
    for i in 0..dfs.n {
        if !dfs.visited[i] {
            dfs.dfs(i);
        }
    }
    dfs.res
}