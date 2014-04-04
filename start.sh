#!/bin/sh
erl -pa ebin deps/*/ebin -config zabbix_sender -s zabbix_sender
