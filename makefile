CC = gcc
CFLAGS = -g
TEST_LDFLAGS = -lcriterion

SRC=src
TEST=src/test

SRC_OBJS=$(SRC)/parser.tab.o $(SRC)/scanner.o $(SRC)/aes.o $(SRC)/geom.o $(SRC)/cta.o $(SRC)/qual.o $(SRC)/scale.o $(SRC)/keyword.o
TEST_OBJS=$(TEST)/test_sgl_to_cgs.o $(TEST)/test_aes.o $(TEST)/test_geom.o $(TEST)/test_cta.o $(TEST)/test_qual.o $(TEST)/test_scale.o $(TEST)/test_keyword.o $(TEST)/stubs.o

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

$(TEST)/test_sgl_to_cgs.o : $(TEST)/test_sgl_to_cgs.c $(SRC)/aes.h $(SRC)/direction.h $(SRC)/sgl_to_cgs.h
	$(CC) $(CFLAGS) -o $(TEST)/test_sgl_to_cgs.o -c $(TEST)/test_sgl_to_cgs.c -I./$(SRC)

$(TEST)/test_aes.o : $(TEST)/test_aes.c $(SRC)/aes.h $(SRC)/keyword.h
	$(CC) $(CFLAGS) -o $(TEST)/test_aes.o -c $(TEST)/test_aes.c -I./$(SRC)

$(TEST)/test_geom.o : $(TEST)/test_geom.c $(SRC)/geom.h $(SRC)/keyword.h
	$(CC) $(CFLAGS) -o $(TEST)/test_geom.o -c $(TEST)/test_geom.c -I./$(SRC)

$(TEST)/test_cta.o : $(TEST)/test_cta.c $(SRC)/cta.h $(SRC)/keyword.h
	$(CC) $(CFLAGS) -o $(TEST)/test_cta.o -c $(TEST)/test_cta.c -I./$(SRC)

$(TEST)/test_qual.o : $(TEST)/test_qual.c $(SRC)/qual.h $(SRC)/keyword.h
	$(CC) $(CFLAGS) -o $(TEST)/test_qual.o -c $(TEST)/test_qual.c -I./$(SRC)

$(TEST)/test_scale.o : $(TEST)/test_scale.c $(SRC)/scale.h $(SRC)/keyword.h
	$(CC) $(CFLAGS) -o $(TEST)/test_scale.o -c $(TEST)/test_scale.c -I./$(SRC)

$(TEST)/test_keyword.o : $(TEST)/test_keyword.c $(SRC)/keyword.h
	$(CC) $(CFLAGS) -o $(TEST)/test_keyword.o -c $(TEST)/test_keyword.c -I./$(SRC)

$(TEST)/stubs.o : $(TEST)/stubs.c
	$(CC) $(CFLAGS) -o $(TEST)/stubs.o -c $(TEST)/stubs.c

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.so
	rm -f $(TEST)/*.o
	rm -f $(TEST)/test
