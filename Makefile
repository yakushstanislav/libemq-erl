LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
LIBNAME=emq-erl
VERSION=1.0
INSTALL_DIR=${LIBDIR}/${LIBNAME}-${VERSION}

all: compile

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -f ebin/*.beam

install: compile
	mkdir -p $(INSTALL_DIR)/ebin ${INSTALL_DIR}/include
	for i in ebin/*.beam include/emq.hrl; do install $$i $(INSTALL_DIR)/$$i ; done
