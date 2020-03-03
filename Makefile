install:
	@echo "installing"
	bash install.sh
	source ~/.bashrc
update:
	@echo "updating"
	git pull
	bash update.sh
	source ~/.bashrc
