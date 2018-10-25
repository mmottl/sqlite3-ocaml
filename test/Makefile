TARGETS = \
  $(addsuffix .bc, test_agg test_db test_error test_exec test_fun test_stmt)

.PHONY: all clean

all:
	@dune build $(TARGETS)

clean:
	@dune clean
