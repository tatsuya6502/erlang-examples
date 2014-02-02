
REBAR?=./rebar

OTPREL=$(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
PLT=$(HOME)/.dialyzer_plt.$(OTPREL)

ERLDIRS?=./ebin

.PHONY: all clean compile xref test eunit compile-for-eunit build-plt dialyze

all: compile

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: eunit

eunit: compile-for-eunit
	$(REBAR) eunit skip_deps=true

compile-for-eunit:
	$(REBAR) compile eunit compile_only=true

$(PLT):
	@echo "building: $(PLT) ..."
	-dialyzer --build_plt --output_plt $(PLT) --apps \
		erts \
		kernel \
		stdlib \
		crypto \
		mnesia \
		sasl \
		common_test \
		eunit

build-plt: $(PLT)

dialyze: build-plt clean compile
	dialyzer --plt $(PLT) -Wunmatched_returns -r $(ERLDIRS)
