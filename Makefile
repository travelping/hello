#    __                        __      _
#   / /__________ __   _____  / /___  (_)___  ____ _
#  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
# / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
# \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
#                           /_/            /____/
#
# Copyright (c) Travelping GmbH <info@travelping.com>

ERL  = erl
ERLC = erlc

SRC_DIR     = $(CURDIR)/src
EBIN_DIR    = $(CURDIR)/ebin
INCLUDE_DIR = $(CURDIR)/include

.PHONY: all clean doc shell

all:
	$(ERL) -pa $(EBIN_DIR) -noinput \
	-eval "case make:all() of up_to_date -> halt(0); error -> halt(1) end."

clean:
	rm -f $(EBIN_DIR)/*.beam

shell: all
	$(ERL) -pa $(EBIN_DIR)
