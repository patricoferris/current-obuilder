((from ocaml/opam:alpine-ocaml-4.14)
 (workdir /src)
 (user (uid 1000) (gid 1000))
 (copy (src current-obuilder.opam) (dst ./))
 (run (shell "opam pin add -yn ."))
 (run
  (network host)
  (shell "opam install --deps-only -t ."))
 (run (shell "opam exec -- dune build")))
