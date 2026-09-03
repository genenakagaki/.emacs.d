FROM node:22-alpine

WORKDIR /app

USER root
RUN mkdir -p node_modules && chown -R node:node /app

# Switch back to the unprivileged user
USER node

CMD ["npm", "run", "docs:dev"]