zabbix_sender
=============

Simple native Zabbix_sender for Erlang applications

1. For starting sender with your application you should include it in your app.src file.
2. In config file zabbix_sender.config you need insert list of params:
  zabbix_host - IP adress of Zabbix server host;
  zabbix_port - listening port for trappers in Zabbix server (default value 10050);
  sender_host - name of your application host in Zabbix server web interface. 
	send_file_path - absolute path for file with trappers (optional parameter, need only for using function     send_from_file/1, send_from_file/0.
3. In Zabbix server web interface add trappers (additional info https://www.zabbix.com/documentation/2.0/manual/config/items/itemtypes/trapper)
4. Use functions send_item/1, send_list_items/1 for sending pairs {key, value} to Zabbix from default hostname in zabbix_sender.config.
5. Use functions send_item/2, send_list_items/2 for sending pairs {key, value} to Zabbix from custom hostname different with zabbix_sender.config.
6. Also if you want store pairs in file and after them send all file with trappers to Zabbix use functions  send_from_file/0 for default file from zabbix_sender.config or send_from_file/1 for custom filename.
