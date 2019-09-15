FROM alpine:3.10 AS builder

RUN apk update && \
    apk upgrade && \
    apk add autoconf curl guile-dev make

RUN curl -LO https://files.dthompson.us/haunt/haunt-0.2.4.tar.gz && \
    tar xf haunt-0.2.4.tar.gz && \
    cd haunt-0.2.4 && \
    ./configure --prefix=/usr && \
    make && \
    make install

WORKDIR /blog

COPY . .

RUN haunt build

FROM nginx:1.17-alpine AS runner

COPY --from=builder /blog/site /usr/share/nginx/html