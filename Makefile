install:
	@echo "installing"
	bash install.sh
	source ~/.bashrc
update:
	@echo "updating"
	git pull
	bash update.sh
push:
	@echo "pushing updates"
	bash updateLocal.sh
	git add .
	git commit -m 'updated dotfiles'
	git push -u origin master
