PROJ_PATH=./compiler
CARGO_PATH=./cargo
LEVEL?=lv1
run:
	mkdir -p $(CARGO_PATH)/registry
	docker run -it --rm -w /root/compiler -v $(CARGO_PATH)/registry:/root/.cargo/registry -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev \
		bash -c "cargo run -- -koopa hello.c -o hello.koopa"

test:
	mkdir -p $(CARGO_PATH)/registry
	docker run -it --rm -v $(CARGO_PATH)/registry:/root/.cargo/registry -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev \
		autotest -koopa -s $(LEVEL) /root/compiler