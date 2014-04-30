# Makefile - for the emacs setup
# GNU make is required

all: org ess
org:
	$(MAKE) -C org
ess:
	$(MAKE) -C vendor/ess




