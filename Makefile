.PHONY: deploy
deploy: lint test build
	netlify deploy --prod

.PHONY: build
build:
	pnpm build

.PHONY: test
test:
	pnpm test

.PHONY: test-watch
test-watch:
	pnpm test:watch

.PHONY: dev
dev:
	pnpm dev

.PHONY: lint
lint:
	pnpm lint

.PHONY: format
format:
	pnpm format
