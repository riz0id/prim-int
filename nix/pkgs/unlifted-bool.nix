{ fetchTarball ? builtins.fetchTarball }:

let 
  pkg = "unlifted-bool";
  rev = "1.1.1";
in fetchTarball {
  # unlifted-bool-0.1.1.0
  url    = "https://github.com/riz0id/${pkg}/releases/download/${rev}/${pkg}-${rev}.tar.gz";
  sha256 = "0w7nd3m1x583l1jnnlxv9v3pq8d5c1anp2m55is4m8d6h2h331dq";
}