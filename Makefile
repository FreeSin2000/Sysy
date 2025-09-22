PROJ_PATH=./compiler
CARGO_PATH=./cargo
run:
	mkdir -p $(CARGO_PATH)/registry
	docker run -it --rm -w /root/compiler -v $(CARGO_PATH)/registry:/root/.cargo/registry -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev bash
