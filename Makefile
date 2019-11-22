.PHONY: convert test

test:
	cd ./exercise2 && fsharpi ./test.fsx

convert:
	find . -type f -name '*.log' -exec sh -c 'python ./trace/trace.py $$0 > $$0.trace' {} \;