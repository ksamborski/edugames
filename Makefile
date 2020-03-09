.PHONY: multiplication
multiplication:
	@docker-compose up -d app
	@docker-compose exec app elm make src/Multiplication.elm --output multiplication.js --debug

.PHONY: multiplication_release
multiplication_release:
	@docker-compose up -d app
	@docker-compose exec app elm make src/Multiplication.elm --output multiplication.js --optimize
	@docker-compose exec app elm-minify multiplication.js

.PHONY: format
format:
	@docker-compose up -d app
	@docker-compose exec app elm-format --yes src

.PHONY: analyse
analyse:
	@docker-compose up -d app
	@docker-compose exec app elm-analyse
