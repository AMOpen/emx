include ../support/include.mk
 
all: 	$(EBIN_FILES)
 
debug:
	$(MAKE) DEBUG=-DDEBUG
 
clean:
	rm -rf $(EBIN_FILES)
	
test:
	cd ../ebin ; $(ERL) -noshell -run run_test runTest -run init stop; cd ../src


