PROJECT = be2bill
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

include erlang.mk

docs:: edoc
