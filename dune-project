(lang dune 3.20)

(name toyhttpserver)

(generate_opam_files true)

(source
 (github dx3mod/tinyohttp))

(package
 (name toyhttpserver)
 (synopsis "Toy HTTP/1.1 server")
 (depends ocaml)
 (tags
  (http server web)))
