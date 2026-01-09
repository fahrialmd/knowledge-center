# Prerequisite

1. Docker

https://www.docker.com/get-started/

* VScode

https://code.visualstudio.com/

* NodeJS

https://nodejs.org/en/download

# Preparation

## Template Code

To simplify replicating this tutorial, it’s recommended to work with the same code base.

1. Clone this repository

```bash
git clone https://github.com/fahrialmd/captemplateproject.git
```

* Install the dependencies

```java
npm ci
```

Or to install postgres specific dependencies you can use these commands

```java
npm add @sap/cds-dk
npm add @cap-js/postgres
```

* Use cds command to add postgres

```java
cds add postgres
```

* Preview your application, use `cds watch`

> If you encounter this error :
>
> > ❗️ ERROR on server start: ❗️
> >
> > &#x20;Error: Failed loading service implementation from @cap-js/sqlite
>
> Means you need to install sqlite package, use&#x20;
>
> `npm i @cap-js/sqlite`command to install sqlite

* Add java dependency in `srv/pom.xml`

While we're doing this, also delete the H2 dependency.

```xml
<dependency>
    <groupId>com.sap.cds</groupId>
    <artifactId>cds-feature-postgresql</artifactId>
    <scope>runtime</scope>
</dependency>
```

## Docker Container

1. Install Portainer plugin&#x20;

Portainer is a GUI-based container management system. It is entirely optional, but it is a recommended alternative for users who prefer a GUI over CLI Docker commands

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ZmJmYjBhZjE1YTlhOTE2ZGFkMDhiNDU3Y2FlYzY1ZmRfcTVqekRMUUkyMWJ4R0dvV1ZyVWRNM2hFTWVtNVhmSndfVG9rZW46TnBRVWJNcjE5b1B0RG54WGVObWNiQVpDbjRkXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

After installing, click "Live Connect" of local docker environment and go to stack page

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YzBkNzE4ODk0NmU1YzNmM2FjYjIyYjUzYWM3ZDIxMjFfbUNkUzhuN25TY2NITG81RkZEU0tONkxnZ0RpYWFrVWhfVG9rZW46UlVjWGJoQkVKb0YzaG54dXZ6QmNncnYxbkxOXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

* Create docker compose (stack)

Now create docker compose (stack) which consist of postgres container and adminer. Adminer is a lightweight database management system that can be open in browser.

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YzBmMjZlMDZiZDM5NTgxYWQ4NTZkOTQwZDdkMWFiY2VfRUtKYVAxaXZzUHUzdXk2QURmUGVqNEVTZTVndjlDMUVfVG9rZW46UXdObmJVOHdZb3ZKSm14RmVrM2NSTXgwbnplXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

```yaml
services:
  postgresdb:
    image: postgres:latest
    container_name: postgresdb
    restart: always
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: secret
      POSTGRES_DB: mydb
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432" 
  adminer:
    image: adminer:latest
    container_name: adminer
    restart: always
    ports:
      - "1700:8080"
    depends_on:
      - postgresdb
volumes:
  postgres_data:
```

Deploy and check for running status

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=Y2E4ZTAyMmJmMDdjZjcxODA3YmFkYWNjMTUwNTZiYTdfNDhYUmlyS1RvN2hnVFFKY0xZWXAzRFVBVUROejVZVjZfVG9rZW46R3MzM2JydHVJb2N4TXJ4MHJiaGNmU3V5blNjXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

* Login to adminer&#x20;

Open localhost:1700 and login to adminer

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ZDRlZjk3NWUwODI4NjM5ZWZjZjJkYTZlN2M0OTEyYjBfQjVXNGsxcWs1cXNZTnFNV2l2VmEzV1JRdVVpd3psazdfVG9rZW46STZhcWIwcXRJb0g3U1J4dkxWMWM2OHBlbnRmXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

Successfully logged in to postgres db

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NTdkNzYwYmZhNTBkOTE1YTBhZDZhYTk4NDQ4NmZmZjFfTFphUVNiZFppdWRaaDVxbWZ0cmM3eWh0MVRSU2tUTE1fVG9rZW46WGNkcGIzTzlMb2YwWGt4aFZmZ2NWOWJVblBlXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

* Create database&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YzYxYmEwMzEzZmExMjMyMjlmZWI0MmUyNDFjZDMwMWNfS3NCWlpndldIRTN0OGxnb1VuOUhZNno3ZktXaDBmU3JfVG9rZW46VWp3WmJtdk1nb29UZHR4SzlSeGM3MzZkbldoXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ZDA1ZGIzNzZmNTk2MjNiYmI0MGJiOTdhNWY3MmM4MGZfd0gzZGtmZ1JQNHZtUDhoZUdJc0pEWGdsNmsyQllQaUZfVG9rZW46VWxrd2JFemRtb1k5Skd4WW9hM2M0ZTVibnRrXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

# Implementation

1. To connect to postgres container, we need to specify the credentials in the project. Add this syntax to .cdsrc-private.json.

```json
  "requires": {
    "db": {
      "kind": "postgres",
      "impl": "@cap-js/postgres",
      "credentials": {
        "host": "localhost",
        "port": 5432,
        "database": "captest",
        "user": "postgres",
        "password": "secret"
      }
    }
  }
```

Make sure to input the correct credentials according to your container configuration, also make sure you have already created the database "captest"

* Check with `cds env` to see environment variables in your project

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YjQyZjJmMTFlZjdlYjJjMzU0NGFlYWIzOGI5NTNiMTFfemFLb3FqZ2VqcDZ1VkFOMlBZQ3FvbGFtT3psVkRlRWhfVG9rZW46Ulo4d2I5eVdCb3diYWp4Z1BmZmNrSkU3blJPXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

* Edit application.yaml

```yaml
spring:
  datasource:
    password: secret
    embedded-database-connection: none
    driver-class-name: org.postgresql.Driver
    url: jdbc:postgresql://localhost:5432/captest
    username: postgres
  config:
    activate:
      on-profile: default
cds:
  data-source:
    auto-config:
      enabled: true

```

* Modify "command" in srv/pom.xml

```xml
<execution>
    <id>cds.build</id>
    <goals>
        <goal>cds</goal>
    </goals>
    <configuration>
        <commands>
            <command>build --for java</command>
            <command>deploy --to postgres --dry --out
                "${project.basedir}/src/main/resources/schema.sql"</command>
        </commands>
    </configuration>
</execution>
```

* Execute `cds deploy`

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MjE0NzY0YmQwM2NhNTViZjA3Y2Y1N2FjM2FmYjRkMmFfTmdrclRudjlBQ3JaMGhuYlkxakZ0VXNoTXo0TUJNRDVfVG9rZW46SThKWmJZbjRubzNUdjd4NjdKdGN1eXhJbkZjXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

* Check your database for deployed schema

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YzNhMzNiZDllMTFlNGUyNWFhZjlkYWI2NDM5MjIzNDNfd0R5OWNpZHY3VnRDZ0R0TTlNaThCeWdtRllYTzA1NUJfVG9rZW46T3ZpWmJldzJwb2R4d1J4aHFwS2NnUFlGbjJiXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YzlkNDUyOTc4YzAwNDI0N2NlNzg3M2M3N2UzODhmNjZfbEtZM3lZREpSdXdQdlpPd0xFOFFCQzFENXZzYlFhWEFfVG9rZW46Qm00ZWJ4VkRGb1BpdWZ4NzE0dWNCTGtVbmRkXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

* Run the application `mvn spring-boot:run`

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=OGQyYmVjZjEyMmRmMDFkYmMzMWE5YTk0NTYyMjI4M2VfbVpCZlpHMzJqVG4yeUhCSXBtUXFYTDdZVkllZkd4NjZfVG9rZW46WkdSOGJLTEl0bzg5T2J4emRCbmNzd29SbklmXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MmFjNmFiNWEzN2IzNGY4MmM4YzExYWU2ZGY5YWM2NjRfSUV6U1h4dk9iZHk4Nm5GTDhwd24zSnBncE9mUXJ6NUVfVG9rZW46VWxXY2JIdVdvb3gzYW94NG9sc2NXOGxibjRlXzE3Njc5MjQzMzM6MTc2NzkyNzkzM19WNA)

# Reference:

https://community.sap.com/t5/technology-blog-posts-by-sap/run-and-deploy-sap-cap-node-js-or-java-with-postgresql-on-sap-btp-cloud/ba-p/13558467

