TARGETS = \
  $(addsuffix .bc, test_agg test_db test_error test_exec test_fun test_stmt)

.PHONY: all clean

all:
	@jbuilder build --dev $(TARGETS)

clean:
	@jbuilder clean
