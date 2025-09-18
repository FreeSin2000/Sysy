PROJ_PATH=./compiler
run:
	docker run -it --rm -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev bash
