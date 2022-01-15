SHELL=/bin/bash

# no not delete intermediate files
.SECONDARY:

# the directory where the challenge will be created
output_directory=...

# let's define which files are necessary, other files will be created if needed;
# we'll compress the input files with xz and leave `expected.tsv` files uncompressed
# (but you could decide otherwise)
all: $(output_directory)/train/in.tsv.xz $(output_directory)/train/expected.tsv \
     $(output_directory)/dev-0/in.tsv.xz $(output_directory)/dev-0/expected.tsv \
     $(output_directory)/test-A/in.tsv.xz $(output_directory)/test-A/expected.tsv \
     $(output_directory)/README.md \
     $(output_directory)/in-header.tsv \
     $(output_directory)/out-header.tsv
	# always validate the challenge
	geval --validate --expected-directory $(output_directory)

# we need to replace the default README.md, we assume that it
# is kept as challenge-readme.md in the repo with this Makefile;
# note that the title from README.md will be taken as the title of the challenge
# and the first paragraph — as a short description
$(output_directory)/README.md: challenge-readme.md $(output_directory)/config.txt
	cp $< $@

# prepare header files (see above section on headers)
$(output_directory)/in-header.tsv: in-header.tsv $(output_directory)/config.txt
	cp $< $@

$(output_directory)/out-header.tsv: out-header.tsv $(output_directory)/config.txt
	cp $< $@

$(output_directory)/config.txt:
	mkdir -p $(output_directory)
	geval --init --expected-directory $(output_directory) --metric MAIN_METRIC --metric AUXILIARY_METRIC --precision N --gonito-host https://some.gonito.host.net
	# `geval --init` will generate a toy challenge for a given metric(s)
	# ... but we remove the `in/expected.tsv` files just in case
	# (we will overwrite this with our data anyway)
	rm -f $(output_directory)/{train,dev-0,test-A}/{in,expected}.tsv
	rm $(output_directory)/{README.md,in-header.tsv,out-header.tsv}

# a "total" TSV containing all the data, we'll split it later
all-data.tsv.xz: prepare.py some-other-files
	# the data are generated using your script, let's say prepare.py and
	# some other files (of course, it depends on your task);
	# the file will be compressed with xz
	./prepare.py some-other-files | xz > $@

# and now the challenge files, note that they will depend on config.txt so that
# the challenge skeleton is generated first

# The best way to split data into train, dev-0 and test-A set is to do it in a random,
# but _stable_ manner, the set into which an item is assigned should depend on the MD5 sum
# of some field in the input data (a field unlikely to change). Let's assume
# that you created a script `filter.py` that takes as an argument a regular expression that will be applied
# to the MD5 sum (written in the hexadecimal format).

$(output_directory)/train/in.tsv.xz $(output_directory)/train/expected.tsv: all-data.tsv.xz filter.py $(output_directory)/config.txt
	# 1. xzcat for decompression
	# 2. ./filter.py will select 14/16=7/8 of items in a stable random manner
	# 3. tee >(...) is Bash magic to fork the ouptut into two streams
	# 4. cut will select the columns
	# 5. xz will compress it back
	xzcat $< | ./filter.py '[0-9abcd]$' | tee >(cut -f 1 > $(output_directory)/train/expected.tsv) | cut -f 2- | xz > $(output_directory)/train/in.tsv.xz

$(output_directory)/dev-0/in.tsv.xz $(output_directory)/dev-0/expected.tsv: all-data.tsv.xz filter.py $(output_directory)/config.txt
	# 1/16 of items goes to dev-0 set
	xzcat $< | ./filter.py 'e$' | tee >(cut -f 1 > $(output_directory)/dev-0/expected.tsv) | cut -f 2- | xz > $(output_directory)/dev-0/in.tsv.xz

$(output_directory)/test-A/in.tsv.xz $(output_directory)/test-A/expected.tsv: all-data.tsv.xz filter.py $(output_directory)/config.txt
	# (other) 1/16 of items goes to test-A set
	xzcat $< | ./filter.py 'f$' | tee >(cut -f 1 > $(output_directory)/test-A/expected.tsv) | cut -f 2- | xz > $(output_directory)/test-A/in.tsv.xz

# wiping out the challenge, if you are desperate
clean:
	rm -rf $(output_directory)
