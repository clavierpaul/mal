# Adapted from the Rust Makefile

UPPER_STEPS = step4_if_fn_do step5_tco step6_file step7_quote step8_macros step9_try stepA_mal
STEPS = step0_repl step1_read_print step2_eval step3_env $(UPPER_STEPS)

all: $(STEPS)

%: %.fs 
	dotnet build projects/$@.fsproj -o build/
	rm -f $@
	ln -s build/$@ $@

STEP1_DEPS = types.fs reader.fs printer.fs
STEP2_DEPS = $(STEP1_DEPS) common.fs state.fs
STEP3_DEPS = $(STEP2_DEPS) env.fs
STEP4_DEPS = $(STEP3_DEPS) core.fs

step0_repl: $(STEP0_DEPS)
step1_read_print step2_eval: $(STEP1_DEPS)
step3_env: $(STEP3_DEPS)
$(UPPER_STEPS): $(STEP4_DEPS)

.PHONY: clean

clean:
	rm -rf build/
	rm -rf projects/obj
	rm -rf projects/bin
	rm -f $(STEPS)
