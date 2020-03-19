.PHONY: multiplication
multiplication:
	@docker-compose up -d app
	@docker-compose exec app elm make src/Multiplication.elm --output demo/multiplication.js --debug

.PHONY: multiplication_release
multiplication_release:
	@docker-compose up -d app
	@docker-compose exec app elm make src/Multiplication.elm --output demo/multiplication.js --optimize
	@docker-compose exec app elm-minify demo/multiplication.js

.PHONY: format
format:
	@docker-compose up -d app
	@docker-compose exec app elm-format --yes src

.PHONY: analyse
analyse:
	@docker-compose up -d app
	@docker-compose exec app elm-analyse

.PHONY: test
test:
	@docker-compose up -d app
	@docker-compose exec app elm-test
