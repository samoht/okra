FROM ocaml/opam:alpine-ocaml-4.14 AS build
RUN sudo mv /usr/bin/opam-2.2 /usr/bin/opam
WORKDIR /src
COPY okra-lib.opam okra.opam .
RUN opam install . --depext-only
RUN opam install . --deps-only --with-test
COPY . .
RUN opam exec -- dune build --profile=dev

FROM alpine
COPY --from=build /src/_build/install/default/bin/okra /okra
RUN apk add gmp
WORKDIR /src
ENTRYPOINT ["/okra"]
