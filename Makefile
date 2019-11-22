.phone convert

convert:
	find . -type f -name '*.log' -exec sh -c 'python trace/trace.py $0 > $0.trace' {} \;