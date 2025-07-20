# Taskerl - Task Dependency Resolution Service

Taskerl is an HTTP service that processes tasks with dependencies and returns them in the correct execution order, either as JSON or as an executable shell script.

## Features

-   Processes tasks with dependencies
-   Detects circular dependencies
-   Returns tasks in proper execution order
-   Output formats:
    -   JSON (with full task details)
    -   Executable shell script (just commands)
-   Runs as a release application with proper supervision

## Requirements

To run Taskerl, you need:

-   Erlang/OTP (version 24 or later recommended)
-   rebar3 (build tool)
-   Git (for version control)
-   (optional) curl or similar HTTP client (for testing API)

## Installation

```bash
make init
```

## Usage

### Running the service

```bash
make run
```

### Stopping the service

```bash
make stop
```

### Testing

```bash
make test
```

### Cleaning build artifacts

```bash
make clean
```

## Configuration

The service can be configured via `config/sys.config`:

```erlang
[
    {taskerl, [
        {http_port, 8080}  % Port to listen on
    ]}
].
```

## API Endpoints

### POST `/tasks/json`

Process tasks and return JSON result with sorted tasks

**Request Format:**

```json
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1",
            "requires": ["task-3"]
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1",
            "requires": ["task-1"]
        }
    ]
}
```

**Response:**

```json
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1"
        }
    ]
}
```

### POST `/tasks/shell`

Process tasks and return executable shell script

**Request Format:**

```json
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1",
            "requires": ["task-3"]
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1",
            "requires": ["task-1"]
        }
    ]
}
```

**Response:**

```bash
#!/usr/bin/env bash
touch /tmp/file1
echo 'Hello World!' > /tmp/file1
cat /tmp/file1
```

## Example Requests

### 1. Requesting JSON Output

**API Call:**

```bash
curl -X POST -H "Content-Type: application/json" \
    -d '{
        "tasks": [
            {
                "name": "task-1",
                "command": "touch /tmp/file1"
            },
            {
                "name": "task-2",
                "command": "cat /tmp/file1",
                "requires": ["task-3"]
            },
            {
                "name": "task-3",
                "command": "echo '\''Hello World!'\'' > /tmp/file1",
                "requires": ["task-1"]
            },
            {
                "name": "task-4",
                "command": "rm /tmp/file1",
                "requires": ["task-2", "task-3"]
            }
        ]
    }' \
    http://localhost:8080/tasks/json
```

**Or with test file:**

```bash
# JSON output using test file
curl -X POST -H "Content-Type: application/json" \
    -d @apps/taskerl/test/inputs/multiple_tasks.json \
    http://localhost:8080/tasks/json
```

**Expected API Output:**

```json
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1"
        },
        {
            "name": "task-4",
            "command": "rm /tmp/file1"
        }
    ]
}
```

### 2. Requesting Shell Script Output

**API Call:**

```bash
curl -X POST -H "Content-Type: application/json" \
    -d '{
        "tasks": [
            {
                "name": "task-1",
                "command": "touch /tmp/file1"
            },
            {
                "name": "task-2",
                "command": "cat /tmp/file1",
                "requires": ["task-3"]
            },
            {
                "name": "task-3",
                "command": "echo '\''Hello World!'\'' > /tmp/file1",
                "requires": ["task-1"]
            },
            {
                "name": "task-4",
                "command": "rm /tmp/file1",
                "requires": ["task-2", "task-3"]
            }
        ]
    }' \
    http://localhost:8080/tasks/shell
```

**Or with test file:**

```bash
# JSON output using test file
curl -X POST -H "Content-Type: application/json" \
    -d @apps/taskerl/test/inputs/multiple_tasks.json \
    http://localhost:8080/tasks/shell
```

**Expected API Output:**

```bash
#!/usr/bin/env bash
touch /tmp/file1
echo 'Hello World!' > /tmp/file1
cat /tmp/file1
rm /tmp/file1
```
