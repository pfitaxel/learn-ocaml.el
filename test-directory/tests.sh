#!bin/sh
emacs --batch --eval '(message (pp (+ 2 2)))'

# test for Download last uploaded file 
echo "let plus = ( + )">expected-demo.ml
echo -ne '\ntest\n' | learn-ocaml-client --server=http://localhost --html --id=demo expected-demo.ml 
emacs -l learn-ocaml.el -f learn-ocaml-download-server-file :id "demo" &
sleep 10
diff demo.ml expected-demo.ml