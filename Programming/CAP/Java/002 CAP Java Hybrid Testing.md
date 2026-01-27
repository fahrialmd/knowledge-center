# Purpose
CAP Java hybrid testing means using BTP services of HANA DB, XSUAA and Approuter but running the application locally
# Reference
1. [[001 CAP Java Quickstart]]
# Implementation
## 1. Bind HANA DB
- Add HANA dependency
```BASH
cds add hana
```
- Replace database migration command pointing to HANA in `srv/pom.xml`
```XML
                    <execution>
                        <id>cds.build</id>
                        <goals>
                            <goal>cds</goal>
                        </goals>
                        <configuration>
                            <commands>
                                <command>build --for java</command>
                                <command>deploy --to hana --dry &gt; srv/src/main/resources/schema.sql</command>
                            </commands>
                        </configuration>
                    </execution>
```
- Enable data source auto config and remove H2 as SQL initial platform in `srv\src\main\resources\application.yaml`
```YAML
spring:
  config.activate.on-profile: default
  # sql.init.platform: h2
cds:
  data-source.auto-config.enabled: true
```
# Result
