PROJ_PATH=./compiler
CARGO_PATH=./cargo
LEVEL?=lv1
MODE?=koopa

debug:
	mkdir -p $(CARGO_PATH)/registry
	docker run -it --rm -w /root/compiler -v $(CARGO_PATH)/registry:/root/.cargo/registry -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev \
		bash
run:
	mkdir -p $(CARGO_PATH)/registry
	docker run -it --rm -w /root/compiler -v $(CARGO_PATH)/registry:/root/.cargo/registry -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev \
		bash -c "cargo run -- -$(MODE) hello.c -o hello.koopa"

test:
	mkdir -p $(CARGO_PATH)/registry
	docker run -it --rm -v $(CARGO_PATH)/registry:/root/.cargo/registry -v $(PROJ_PATH):/root/compiler maxxing/compiler-dev \
		autotest -$(MODE) -s $(LEVEL) /root/compiler