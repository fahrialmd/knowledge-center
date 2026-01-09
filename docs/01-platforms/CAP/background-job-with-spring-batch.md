# Introduction

## Overview

Spring Batch is a processing framework built for the reliable execution of jobs. It offers reusable functions that are essential for handling large volumes of records, including logging and tracing, transaction management, job statistics, restart capabilities, skip logic, and resource management.

In the context of our project, Spring Batch is important because it allows us to efficiently process large-scale data through background jobs, preventing performance bottlenecks and avoiding the risk of overloading the frontend with heavy data operations.

## Conceptual Overview

### Architecture

This is the core architecture of spring batch

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MjI3NGJmNTJmYmM5NTRmZjI3NzE5OTllYzljZGVhYmFfYmp1S1ozenZMMXlYZ1A4bjhsWnIybUJqVUhWakZqMnJfVG9rZW46V2lPSGJPRDQzb1EzUmV4bVpJNGNjZ2VIbjBiXzE3Njc5MjQwNjc6MTc2NzkyNzY2N19WNA)

**JobLauncher**

* The entry point for starting a batch job.

* A `JobLauncher` is responsible for triggering the execution of a `Job` with specific parameters.

**Job**

* Represents the *batch process as a whole*.

* A job is composed of one or more **Steps**.

* Each execution of a job is recorded in the **JobRepository**.

**Step**

* A single, independent phase of a job.

* A step usually contains **ItemReader**, **ItemProcessor**, and **ItemWriter** to implement the *read-process-write* pattern.

**ItemReader**

* Reads input data (e.g., from CSV, Excel, database, or API).

* Provides data one item (or chunk) at a time to the processor.

**ItemProcessor** *(optional, 0..1)*

* Applies business logic such as validation, transformation, or filtering.

* It’s optional — if you don’t need to transform or validate, you can skip this.

**ItemWriter**

* Persists the processed data (e.g., into CAP entities, relational DB, or external service).

* Works with chunks (e.g., commit every 100 records).

**JobRepository**

* Stores metadata about jobs and steps, such as execution status, start/end time, failure reasons, and restart points.

* Ensures reliability: if a job fails, Spring Batch can resume from where it left off.

### Workflow

* A user action (like uploading a file in CAP/Fiori) triggers the JobLauncher.

* The JobLauncher executes a Job, which is broken down into Steps.

* Each step follows the Reader → Processor → Writer pipeline.

* The JobRepository tracks everything: which jobs ran, their parameters, whether they succeeded or failed, and allows restarts if needed.

# Prerequisites

## Required Software

* SAP BTP Trial Account

* SAP HANA Cloud database

* Cloud Foundry CLI

* JAVA JDK

* VSCode

* Git

## Configuration

### BTP Trial account

Apply for trial account in this link

https://account.hana.ondemand.com/

### Cloud Foundry CLI

Install from here: https://github.com/cloudfoundry/cli

### SAP Hana Cloud Service

Hana Cloud is the database we use for this batch job, please refer to this document to bind HANA with CAP project.

[ Advanced CAP Training](https://u0vocx8xrmg.feishu.cn/docx/LasidJtgIo38xNxUqlzcB26cnee)

### Clone Git Repository

Clone from this repository below and use the excel-batch-upload-v1.0.0 tag. This tag already has the minimal code for CDS design and the generic Excel upload feature. We'll add Spring Batch from this step.

```bash
git clone https://github.com/fahrialmd/captemplateproject.git --branch excel-batch-upload-v1.0.1
```

Or just download the source code

Next you need to `cf login`, `cds deploy`,&#x20;

### Create vscode debug launch configuration file

Create folder named `.vscode` in root project and add these files. This will enable us debug and backend hot reload capabilities in CAP project

```bash
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "command": "npx cds bind --exec mvn spring-boot:run",
            "name": "cds bind hybrid",
            "request": "launch",
            "type": "node-terminal"
        },
        {
            "type": "java",
            "name": "Spring Boot-Application",
            "request": "launch",
            "cwd": "${workspaceFolder}",
            "mainClass": "com.customer.captemplateproject.Application",
            "projectName": "captemplateproject",
            "preLaunchTask": "cds_bind"
        }
    ]
}
```

```json
{
    // See https://go.microsoft.com/fwlink/?LinkId=733558
    // for the documentation about the tasks.json format
    "version": "2.0.0",
    "tasks": [
        {
            "label": "cds_bind",
            "type": "shell",
            "command": "cds bind --exec '--' node ./writecfenv.js",
            "isBackground": false
        }
    ]
}
```

When you ready to run the project, choose this option and click the run button.

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MTM1MzkzYTdmZTE2NjY3MmFjMmJlNDVjNzY3YjU1NjdfQmkwNjgzcXowZkViS0pja3ZzR1A1UUNDOFkyR1RJcjJfVG9rZW46U2dObWJsY1VCb2ZLYUp4MHhwZWNpQWJHbkJQXzE3Njc5MjQwNjc6MTc2NzkyNzY2N19WNA)

# Implementation Steps

## Add spring batch dependency

Add this dependency to your srv/pom.xml

```bash
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-batch</artifactId>
</dependency>
```

## Modify application.yaml

Add this spring batch configuration in application.yaml

```yaml
spring:
  config.activate.on-profile: default
  web.resources.static-locations: file:./app/
  batch:
    job:
      enabled: false  # Prevent auto-start of batch jobs
    jdbc:
      initialize-schema: never  # We use HANA artifacts instead
      table-prefix: BATCH_  # Match our HANA table names
---
management:
  endpoint:
    health:
      show-components: always
      probes.enabled: true
  endpoints:
    web:
      exposure:
        include: health
  health:
    defaults.enabled: false
    ping.enabled: true
    db.enabled: true

```

## Create HANA database table artifact for spring batch job

Spring Batch relies on fixed table names such as `BATCH_JOB_INSTANCE` and `BATCH_JOB_EXECUTION`. This differs from CDS entities, which typically get transformed with namespace prefixes (e.g., `COM_CUSTOMER_CAPTEMPLATEPROJECT_BATCHJOBINSTANCE`).&#x20;

Because of these prefixes, Spring Batch will not recognize the tables. To resolve this, HANA artifacts allow you to create tables using the exact names that Spring Batch expects.

```sql
COLUMN TABLE BATCH_JOB_EXECUTION_CONTEXT (
        JOB_EXECUTION_ID BIGINT NOT NULL PRIMARY KEY,
        SHORT_CONTEXT VARCHAR(2500) NOT NULL,
        SERIALIZED_CONTEXT CLOB
);
```

```sql
COLUMN TABLE BATCH_JOB_EXECUTION_PARAMS (
        JOB_EXECUTION_ID BIGINT NOT NULL,
        PARAMETER_NAME VARCHAR(100) NOT NULL,
        PARAMETER_TYPE VARCHAR(100) NOT NULL,
        PARAMETER_VALUE VARCHAR(2500),
        IDENTIFYING VARCHAR(1) NOT NULL
);
```

```bash
SEQUENCE BATCH_JOB_EXECUTION_SEQ START WITH 1 INCREMENT BY 1;
```

```sql
COLUMN TABLE BATCH_JOB_EXECUTION (
        JOB_EXECUTION_ID BIGINT  NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
        VERSION BIGINT,
        JOB_INSTANCE_ID BIGINT NOT NULL,
        CREATE_TIME TIMESTAMP NOT NULL,
        START_TIME TIMESTAMP DEFAULT NULL,
        END_TIME TIMESTAMP DEFAULT NULL,
        STATUS VARCHAR(10),
        EXIT_CODE VARCHAR(2500),
        EXIT_MESSAGE VARCHAR(2500),
        LAST_UPDATED TIMESTAMP
);
```

```sql
COLUMN TABLE BATCH_JOB_INSTANCE (
        JOB_INSTANCE_ID BIGINT  NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
        VERSION BIGINT,
        JOB_NAME VARCHAR(100) NOT NULL,
        JOB_KEY VARCHAR(32) NOT NULL
) ;
```

```bash
SEQUENCE BATCH_JOB_SEQ START WITH 1 INCREMENT BY 1;
```

```sql
COLUMN TABLE BATCH_STEP_EXECUTION_CONTEXT (
        STEP_EXECUTION_ID BIGINT NOT NULL PRIMARY KEY,
        SHORT_CONTEXT VARCHAR(2500) NOT NULL,
        SERIALIZED_CONTEXT CLOB
);
```

```bash
SEQUENCE BATCH_STEP_EXECUTION_SEQ START WITH 1 INCREMENT BY 1;
```

```sql
COLUMN TABLE BATCH_STEP_EXECUTION (
        STEP_EXECUTION_ID BIGINT  NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
        VERSION BIGINT NOT NULL,
        STEP_NAME VARCHAR(100) NOT NULL,
        JOB_EXECUTION_ID BIGINT NOT NULL,
        CREATE_TIME TIMESTAMP NOT NULL,
        START_TIME TIMESTAMP DEFAULT NULL,
        END_TIME TIMESTAMP DEFAULT NULL,
        STATUS VARCHAR(10),
        COMMIT_COUNT BIGINT,
        READ_COUNT BIGINT,
        FILTER_COUNT BIGINT,
        WRITE_COUNT BIGINT,
        READ_SKIP_COUNT BIGINT,
        WRITE_SKIP_COUNT BIGINT,
        PROCESS_SKIP_COUNT BIGINT,
        ROLLBACK_COUNT BIGINT,
        EXIT_CODE VARCHAR(2500),
        EXIT_MESSAGE VARCHAR(2500),
        LAST_UPDATED TIMESTAMP
);
```

```bash
constraint JOB_EXEC_CTX_FK 
on BATCH_JOB_EXECUTION_CONTEXT 
foreign key (JOB_EXECUTION_ID) references BATCH_JOB_EXECUTION(JOB_EXECUTION_ID)
```

```bash
constraint JOB_EXEC_PARAMS_FK 
on BATCH_JOB_EXECUTION_PARAMS 
foreign key (JOB_EXECUTION_ID) references BATCH_JOB_EXECUTION(JOB_EXECUTION_ID)
```

```bash
constraint JOB_EXEC_STEP_FK 
on BATCH_STEP_EXECUTION
foreign key (JOB_EXECUTION_ID) references BATCH_JOB_EXECUTION(JOB_EXECUTION_ID)
```

```bash
constraint JOB_INST_EXEC_FK 
on BATCH_JOB_EXECUTION 
foreign key (JOB_INSTANCE_ID) references BATCH_JOB_INSTANCE(JOB_INSTANCE_ID)
```

```bash
INDEX JOB_INST_UN 
ON BATCH_JOB_INSTANCE (JOB_NAME, JOB_KEY);
```

```bash
constraint STEP_EXEC_CTX_FK 
on BATCH_STEP_EXECUTION_CONTEXT 
foreign key (STEP_EXECUTION_ID) references BATCH_STEP_EXECUTION(STEP_EXECUTION_ID)
```

## Create spring batch entity in schema

Create CDS entity for monitoring via OData APIs. If you had existing cds design you can add these below in the same file `./db/schema.cds`.&#x20;

```yaml
...
@cds.persistence.skip
entity job_instance {
    key JOB_INSTANCE_ID : Int64;
        VERSION         : Int64;
        JOB_NAME        : String(100);
        JOB_KEY         : String(36);
        to_Executions   : Association to many job_execution
                              on JOB_INSTANCE_ID = to_Executions.JOB_INSTANCE_ID;
}

@cds.persistence.skip
entity job_execution {
    key JOB_EXECUTION_ID  : Int64;
        VERSION           : Int64;
        JOB_INSTANCE_ID   : Int64;
        CREATE_TIME       : Timestamp;
        START_TIME        : Timestamp;
        END_TIME          : Timestamp;
        STATUS            : String(10);
        EXIT_CODE         : String(2500);
        EXIT_MESSAGE      : String(2500);
        LAST_UPDATED      : Timestamp;

        to_Instance       : Association to one job_instance
                                on JOB_INSTANCE_ID = to_Instance.JOB_INSTANCE_ID;

        to_Context        : Association to one job_execution_context
                                on JOB_EXECUTION_ID = to_Context.JOB_EXECUTION_ID;

        to_Params         : Association to many job_execution_params
                                on JOB_EXECUTION_ID = to_Params.JOB_EXECUTION_ID;

        to_StepExecutions : Association to many step_execution
                                on JOB_EXECUTION_ID = to_StepExecutions.JOB_EXECUTION_ID;
}

@cds.persistence.skip
entity job_execution_context {
    key JOB_EXECUTION_ID   : Integer64;
        SHORT_CONTEXT      : String(2500);
        SERIALIZED_CONTEXT : LargeString;

        to_Executions      : Association to job_execution
                                 on to_Executions.JOB_EXECUTION_ID = JOB_EXECUTION_ID;
}

@cds.persistence.skip
entity job_execution_params {
    key JOB_EXECUTION_ID : Integer64;
    key PARAMETER_NAME   : String(100);
        PARAMETER_TYPE   : String(100);
        PARAMETER_VALUE  : String(2500);
        IDENTIFYING      : String(1);

        to_Executions    : Association to job_execution
                               on to_Executions.JOB_EXECUTION_ID = JOB_EXECUTION_ID;
}

@cds.persistence.skip
entity step_execution {
    key STEP_EXECUTION_ID       : Integer64;
        VERSION                 : Integer64;
        STEP_NAME               : String(100);
        JOB_EXECUTION_ID        : Integer64;
        CREATE_TIME             : Timestamp;
        START_TIME              : Timestamp;
        END_TIME                : Timestamp;
        STATUS                  : String(10);
        COMMIT_COUNT            : Integer64;
        READ_COUNT              : Integer64;
        FILTER_COUNT            : Integer64;
        WRITE_COUNT             : Integer64;
        READ_SKIP_COUNT         : Integer64;
        WRITE_SKIP_COUNT        : Integer64;
        PROCESS_SKIP_COUNT      : Integer64;
        ROLLBACK_COUNT          : Integer64;
        EXIT_CODE               : String(2500);
        EXIT_MESSAGE            : String(2500);
        LAST_UPDATED            : Timestamp;

        to_Executions           : Association to job_execution
                                      on to_Executions.JOB_EXECUTION_ID = JOB_EXECUTION_ID;
        to_stepExecutionContext : Association to step_execution_context
                                      on to_stepExecutionContext.STEP_EXECUTION_ID = STEP_EXECUTION_ID;
}

@cds.persistence.skip
entity step_execution_context {
    key STEP_EXECUTION_ID  : Integer64;
        SHORT_CONTEXT      : String(2500);
        SERIALIZED_CONTEXT : LargeString;

        to_stepExecution   : Association to step_execution
                                 on to_stepExecution.STEP_EXECUTION_ID = STEP_EXECUTION_ID;
}
```

## Create spring batch configuration files

This is the configuration files we need to integrate spring batch with HANA database. Create new `BatchConfiguration.java` files that defines how jobs are structured and executed. Create in `../com/customer.captemplateproject/confi/batchConfiguration.java`

```typescript
package com.customer.captemplateproject.config;

import java.util.Map;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.support.TaskExecutorJobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.transaction.PlatformTransactionManager;

@Configuration
public class BatchConfiguration {

    @Bean
    public JobLauncher asyncJobLauncher(JobRepository jobRepository) throws Exception {
        TaskExecutorJobLauncher jobLauncher = new TaskExecutorJobLauncher();
        jobLauncher.setJobRepository(jobRepository);
        jobLauncher.setTaskExecutor(new SimpleAsyncTaskExecutor());
        jobLauncher.afterPropertiesSet();
        return jobLauncher;
    }

    @Bean
    public Step excelProcessingStep(
            JobRepository jobRepository,
            @Qualifier("tx-db") PlatformTransactionManager transactionManager,
            ItemReader<Map<Integer, String>> reader,
            ItemProcessor<Map<Integer, String>, Map<Integer, String>> processor,
            ItemWriter<Map<Integer, String>> writer) {

        return new StepBuilder("excelProcessingStep", jobRepository)
                .<Map<Integer, String>, Map<Integer, String>>chunk(5, transactionManager)
                .reader(reader)
                .processor(processor)
                .writer(writer)
                .build();
    }

    @Bean
    public Job excelUploadJob(JobRepository jobRepository, Step excelProcessingStep) {
        return new JobBuilder("excelUploadJob", jobRepository)
                .start(excelProcessingStep)
                .build();
    }
}
```

## Create spring batch workers

Spring Batch uses a pipeline of specialized components to process large datasets efficiently. These components Reader, Processor, and Writer work together. Each is responsible for one distinct task: reading input, validating or transforming it, and finally writing results to a target system.

* The Reader supplies raw data,

* The Processor ensures quality,

* The Writer persists results in bulk/batch.

Create this file below in`../com/customer.captemplateproject/confi/BatchInputReader.java`

```typescript
package com.customer.captemplateproject.batch;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.springframework.batch.item.ItemReader;
import org.springframework.stereotype.Component;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import com.alibaba.excel.util.ListUtils;

@Component
public class BatchInputReader implements ItemReader<Map<Integer, String>> {

    // Create iterator variable
    private Iterator<Map<Integer, String>> dataIterator;

    // Method to receives the uploaded Excel file as a stream of bytes
    public void setInputStream(InputStream inputStream) {
        // Create empty list to store all Excel rows
        List<Map<Integer, String>> data = ListUtils.newArrayList();
        // Read excel file using EasyExcel library
        readExcelFile(inputStream, data);
        // Create iterator
        this.dataIterator = data.iterator();
    }

    // Method for spring batch's "read"
    @Override
    public Map<Integer, String> read() {
        // Check if more row available next
        if (dataIterator != null && dataIterator.hasNext()) {
            // return next row
            return dataIterator.next();
        }
        // Return null as a sign of the end of data
        return null;
    }

    // Method to read excel file using EasyExcel
    private void readExcelFile(InputStream inputStream, List<Map<Integer, String>> data) {
        EasyExcel.read(inputStream, new AnalysisEventListener<Map<Integer, String>>() {
            // Method called by EasyExcel for every row in the Excel file
            @Override
            public void invoke(Map<Integer, String> rowData, AnalysisContext context) {
                // Append excel row into "data" variable
                data.add(rowData);
            }

            // Method called once when EasyExcel finishes reading the entire file
            @Override
            public void doAfterAllAnalysed(AnalysisContext context) {
                // Do nothing for now
            }
        }).sheet().doRead();
    }
}
```

```typescript
package com.customer.captemplateproject.batch;

import java.util.Map;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.stereotype.Component;

@Component
public class BatchInputProcessor implements ItemProcessor<Map<Integer, String>, Map<Integer, String>> {

    @Override
    public Map<Integer, String> process(Map<Integer, String> item) throws Exception {
        // Add any validation or transformation logic here
        // For now, just pass through the data

        // Example validation: check if required fields are present
        if (item.get(0) == null || item.get(0).trim().isEmpty()) {
            // Skip this item if PO Number is missing
            return null;
        }

        return item;
    }
}
```

```java
package com.customer.captemplateproject.batch;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.stereotype.Component;

import com.customer.captemplateproject.service.UploadService;

@Component
public class BatchInputWriter implements ItemWriter<Map<Integer, String>> {

    private final UploadService uploadService;

    public BatchInputWriter(UploadService uploadService) {
        this.uploadService = uploadService;
    }

    @Override
    public void write(Chunk<? extends Map<Integer, String>> chunk) throws Exception {
        List<Map<Integer, String>> items = new ArrayList<>(chunk.getItems());
        if (!items.isEmpty()) {
            uploadService.processPurchaseOrderData(items);
        }
    }
}
```

## Create Batch Service

This is the service layer that acts as the bridge between raw input data and persistent storage. It triggered when the writer worker triggered `uploadService.processPurchaseOrderData(items)`.&#x20;

> While there are multiple valid ways to handle this (e.g., persisting directly in the Writer or delegating to a service), this design reflects what I found to be the most practical and maintainable implementation for this use case. Ultimately, the choice depends on your project needs and constraints

This BatchProcessingService  service is responsible for efficiently handling Purchase Order (PO) data during batch processing. We use caching and custom procesing logic to make sure the raw data from excel can be efficiently and correctly extracted into structured PO according to CDS entity structure.

```java
package com.customer.captemplateproject.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.stereotype.Service;

import cds.gen.purchaseorderservice.PurchaseOrderItems;
import cds.gen.purchaseorderservice.PurchaseOrders;

@Service
@StepScope
public class BatchProcessingService {
    // Map for storing cache of po data
    private final Map<String, PurchaseOrders> purchaseOrderDataCache = new ConcurrentHashMap<>();
    // Cqn service
    private final GenericCqnService genericCqnService;
    // Initial cache checking
    private boolean isCacheInitialized = false;

    public BatchProcessingService(GenericCqnService genericCqnService) {
        this.genericCqnService = genericCqnService;
    }

    /**
     * Main Logic of batch processing
     */
    public void processPurchaseOrderData(PurchaseOrders poData) {
        // Initialize cache
        initializeCache();
        // Cache PO data
        purchaseOrderDataCache.compute(poData.getPoNumber(), (key, existingPO) -> {
            // If there is no existing PO header data, create new also insert to DB
            if (existingPO == null) {
                genericCqnService.insertPurchaseOrderData(poData);
                return poData;
                // Update the items to cache if PO header found
                // also insert the items to DB
            } else {
                // Logic to only insert nonexistence items
                List<PurchaseOrderItems> existingItems = existingPO.getItems();
                List<PurchaseOrderItems> newItems = poData.getItems().stream()
                        .filter(item -> existingPO.getItems().stream()
                                .noneMatch(existing -> existing.getItemNumber().equals(item.getItemNumber())))
                        .toList();
                if (!newItems.isEmpty()) {
                    existingItems.addAll(newItems);
                    genericCqnService.insertPurchaseOrderItem(newItems);
                }
                return existingPO;
            }
        });
    }

    /**
     * Get all processed PO numbers in this step (useful for debugging)
     */
    public Set<String> getProcessedPONumbers() {
        return purchaseOrderDataCache.keySet();
    }

    /**
     * Manual reset - called by step listener or for testing
     */
    public void resetState() {
        purchaseOrderDataCache.clear();
    }

    /**
     * Initialize cache with existing PO numbers from database
     */
    private void initializeCache() {
        if (!isCacheInitialized) {
            List<PurchaseOrders> existingPOs = genericCqnService.initializePODataCaching();
            for (PurchaseOrders po : existingPOs) {
                purchaseOrderDataCache.put(po.getPoNumber(), po);
            }
            isCacheInitialized = true;
        }
    }

}

```

Modify the generic CQN service, add this method below

```c++
public List<PurchaseOrders> initializePODataCaching() {
    var select = Select.from(PurchaseOrders_.class)
            .columns(po -> po.poNumber(),
                    po -> po.items().expand(
                            item -> item.itemNumber()));
    // Run the query and map to PurchaseOrders
    return entityService.selectList(purchaseOrderService, select, PurchaseOrders.class);
}
```

Modify the upload service interface and implementations. This service converts raw excel rows into structured Purchase Order (PO) objects and orchestrates how they are grouped and saved.

```java
package com.customer.captemplateproject.service;

import java.util.List;
import java.util.Map;

import cds.gen.purchaseorderservice.Upload;

public interface UploadService {

    public void processExcelBatchInput(Upload upload);

    public void processPurchaseOrderData(List<Map<Integer, String>> uploadData);
}

```

```java
package com.customer.captemplateproject.service.impl;

import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameter;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.customer.captemplateproject.batch.BatchInputReader;
import com.customer.captemplateproject.exception.BusinessException;
import com.customer.captemplateproject.service.UploadService;

import cds.gen.purchaseorderservice.PurchaseOrderItems;
import cds.gen.purchaseorderservice.PurchaseOrders;
import cds.gen.purchaseorderservice.Upload;

@Service
public class UploadServiceImpl implements UploadService {

    private final GenericCqnService genericCqnService;
    private final JobLauncher asyncJobLauncher;
    private final Job excelUploadJob;
    private final BatchInputReader batchInputReader;
    private final BatchProcessingService batchProcessingService;

    public UploadServiceImpl(
            GenericCqnService genericCqnService,
            @Qualifier("asyncJobLauncher") JobLauncher asyncJobLauncher,
            @Lazy Job excelUploadJob,
            BatchInputReader batchInputReader,
            BatchProcessingService batchProcessingService) {
        this.genericCqnService = genericCqnService;
        this.asyncJobLauncher = asyncJobLauncher;
        this.excelUploadJob = excelUploadJob;
        this.batchInputReader = batchInputReader;
        this.batchProcessingService = batchProcessingService;
    }

    @Override
    public void processExcelBatchInput(Upload upload) {
        InputStream inputStream = upload.getPurchaseOrderData();

        if (inputStream != null) {
            try {
                // Set the input stream in the reader
                batchInputReader.setInputStream(inputStream);

                // Create job parameters
                Map<String, JobParameter<?>> paramMap = new HashMap<>();
                paramMap.put("timestamp", new JobParameter<>(System.currentTimeMillis(), Long.class));
                paramMap.put("fileName", new JobParameter<>("purchase-orders.xlsx", String.class));

                // Launch the batch job
                JobExecution jobExecution = asyncJobLauncher.run(excelUploadJob, new JobParameters(paramMap));

                // You can log the job execution ID or store it for tracking
                System.out.println("Batch job started with execution ID: " + jobExecution.getId());

            } catch (Exception e) {
                throw new BusinessException("Failed to start batch job for Excel processing", e);
            }
        }
    }

    @Override
    public void processPurchaseOrderData(List<Map<Integer, String>> uploadData) {
        try {
            // Group rows by poNumber to separate headers from items
            Map<String, List<Map<Integer, String>>> groupedByPoNumber = uploadData.stream()
                    .collect(Collectors.groupingBy(row -> row.get(0)));
            // Loop the data
            for (Map.Entry<String, List<Map<Integer, String>>> entry : groupedByPoNumber.entrySet()) {
                // Get PO header number
                String poNumber = entry.getKey();
                // Get entire row data
                List<Map<Integer, String>> rows = entry.getValue();
                // Get header data from row
                Map<Integer, String> header = rows.get(0);
                // Create new PO header data
                PurchaseOrders purchaseOrderData = PurchaseOrders.create();
                purchaseOrderData.setPoNumber(header.get(0));
                purchaseOrderData.setPoType(header.get(1));
                purchaseOrderData.setVendorId(header.get(2));
                purchaseOrderData.setCompanyCode(header.get(3));
                purchaseOrderData.setPlant(header.get(4));
                purchaseOrderData.setDocumentDate(LocalDate.parse(header.get(5)));
                purchaseOrderData.setDeliveryDate(LocalDate.parse(header.get(6)));
                purchaseOrderData.setCurrencyCode(header.get(7));
                purchaseOrderData.setTotalAmount(convertToBigDecimal(header.get(8)));
                purchaseOrderData.setDeliveryStatusCode(header.get(9));
                // Create new PO item data
                List<PurchaseOrderItems> purchaseOrderItemData = new ArrayList<>();
                for (Map<Integer, String> row : rows) {
                    PurchaseOrderItems item = PurchaseOrderItems.create();
                    item.setHeaderPoNumber(poNumber);
                    item.setItemNumber(row.get(10));
                    item.setMaterialId(row.get(11));
                    item.setDescription(row.get(12));
                    item.setQuantity(convertToBigDecimal(row.get(13)));
                    item.setUnit(row.get(14));
                    item.setNetPrice(convertToBigDecimal(row.get(15)));
                    item.setNetAmount(convertToBigDecimal(row.get(16)));
                    item.setDeliveryDate(LocalDate.parse(row.get(17)));
                    item.setPlant(row.get(18));
                    purchaseOrderItemData.add(item);
                }
                purchaseOrderData.setItems(purchaseOrderItemData);
                // Add data to BatchProcessingService object for cached based insert logic
                batchProcessingService.processPurchaseOrderData(purchaseOrderData);
            }
        } catch (Exception e) {
            throw new BusinessException("Failed to upload purchase order data: " + e.getMessage(), e);
        }
    }

    private BigDecimal convertToBigDecimal(String input) {
        BigDecimal output = new BigDecimal(input);
        return output;
    }
}

```

## Delete NoModelDataListener.java

This file and folder is no longer used in spring batch framework

## Modify the handler

Simplified `PurchaseOrderServiceHandler` so it now only calls the `UploadService`. This reduces complexity and keeps the handler code clean

```java
package com.customer.captemplateproject.handlers;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameter;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.customer.captemplateproject.batch.BatchInputReader;
import com.customer.captemplateproject.service.UploadService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.cds.CqnService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;

import cds.gen.purchaseorderservice.Upload;
import cds.gen.purchaseorderservice.Upload_;

@Component
@ServiceName("PurchaseOrderService")
public class PurchaseOrderServiceHandler implements EventHandler {

    private final UploadService uploadService;

    public PurchaseOrderServiceHandler(UploadService uploadService) {
        this.uploadService = uploadService;
    }

    @On(entity = Upload_.CDS_NAME, event = CqnService.EVENT_READ)
    public Upload getUploadSingleton() {
        return Upload.create();
    }

    @On(entity = Upload_.CDS_NAME, event = CqnService.EVENT_UPDATE)
    public void handleExcelUpload(CdsUpdateEventContext context, Upload upload) {
        uploadService.processExcelBatchInput(upload);
        context.setResult(Arrays.asList(upload));
    }

}
```

## Demo

This ZIP contains test upload datasets of varying sizes. The largest dataset includes 1,000 rows😊.

Run your app using debug mode and click the import button

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NWY5OGZhMjRkMTRmM2JkMjNmNjBmNWZlOTFhZmU1NjdfYmJ5NlRRREpKaUZCcFBpdVpWRjlKVDllRnBVMGE4bzFfVG9rZW46VHdBOGJpcDFRb0toS054NHkxR2NDcXNhbkJjXzE3Njc5MjQwNjc6MTc2NzkyNzY2N19WNA)

Import file dialog will appear

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NzZjM2IxMTJlOTk4ZTU5YmRmOTg5ODE0ZjZmYjIyYWRfV09YRTNwRmg4Qzk5T3J0VFptOFBHRExwdmFNWDYwUEFfVG9rZW46UG56UGJFajYwbzVBeVd4a24yTGNicHlSbkNnXzE3Njc5MjQwNjc6MTc2NzkyNzY2N19WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NTQxMDg0YzM3MzJkZjIxNmI1MGI2YzA0ZDQ1YWQyMmFfYnBJZXlxckQzbjVYang3dHU2UUEzazN3bmJYQWNmbmhfVG9rZW46V0FPT2J2UFVmb1Q2Sml4dzRKM2NnUWREbmxnXzE3Njc5MjQwNjc6MTc2NzkyNzY2N19WNA)

Depends on your data size it might take a while to upload

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YzVhYmE2YzU1MjU0MjJkMjhhNzQ2NmExYTE4NmFmNmFfZEo5eGNyYVJwOWJqa1dCQ1hRdjJwVUdab29rRk5PVGVfVG9rZW46TUpUR2JxQ3JVb0tOTE94bDVEUWNxNUN5bm5iXzE3Njc5MjQwNjc6MTc2NzkyNzY2N19WNA)

# References

[ CAP集成Spring Batch](https://u0vocx8xrmg.feishu.cn/wiki/VHlrwm5K8iAgp3kULx5cRU91nMc)

[ Spring Batch基础](https://u0vocx8xrmg.feishu.cn/wiki/OK6Jw8zIwitQgHkYLREc8AkTnZe)

https://spring.io/projects/spring-batch

