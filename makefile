CC = gcc
CFLAGS = -g
DEPFLAGS = -MMD -MP
CRITERION_CFLAGS := $(shell pkg-config --cflags criterion)
TEST_LDFLAGS := $(shell pkg-config --libs criterion)

SRC=src
TEST=src/test

SRC_OBJS=$(SRC)/parser.tab.o $(SRC)/scanner.o $(SRC)/aes.o $(SRC)/geom.o $(SRC)/cta.o $(SRC)/qual.o $(SRC)/scale.o $(SRC)/keyword.o $(SRC)/title.o $(SRC)/case.o $(SRC)/cgs_order.o $(SRC)/cgs_free.o
TEST_OBJS=$(TEST)/test_sgl_to_cgs.o $(TEST)/test_aes.o $(TEST)/test_geom.o $(TEST)/test_cta.o $(TEST)/test_qual.o $(TEST)/test_scale.o $(TEST)/test_keyword.o $(TEST)/test_title.o $(TEST)/test_case.o $(TEST)/test_cgs_order.o $(TEST)/stubs.o

test : $(TEST)/test
	$(TEST)/test -j1

$(TEST)/test : r-build $(TEST_OBJS)
	$(CC) $(CFLAGS) -o $(TEST)/test $(TEST_OBJS) $(SRC_OBJS) $(TEST_LDFLAGS)

r-build : parser scanner
	Rscript -e "devtools::load_all()"

parser :
	bison -d -o $(SRC)/parser.tab.c $(SRC)/parser.y

scanner :
	flex -o $(SRC)/scanner.c $(SRC)/scanner.l

$(TEST)/%.o : $(TEST)/%.c
	$(CC) $(CFLAGS) $(DEPFLAGS) $(CRITERION_CFLAGS) -I./$(SRC) -o $@ -c $<

-include $(TEST_OBJS:.o=.d)

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.so
	rm -f $(TEST)/*.o
	rm -f $(TEST)/*.d
	rm -f $(TEST)/test
