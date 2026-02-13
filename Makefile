MAKEFLAGS += -s

# check your exercises
test:
	cabal test

# one-time setup
db-create:
	createdb escalatingesqueleto
	make db-reset
	cabal update

# run whenever you want a freshly populated db
db-reset:
	psql escalatingesqueleto < reset.sql

# connect to the database via `psql`
psql:
	psql -d escalatingesqueleto


# close feedback loop with ghciwatch
ghciwatch:
	ghciwatch \
		--watch exercises \
		--watch lib \
		--watch answers \
		--watch test \
		--enable-eval \
		--clear \
		--error-file ghcid.txt \
		--test-ghci Main.main \
		--command "cabal repl escalating-esqueleto-test"

.PHONY: test ghciwatch