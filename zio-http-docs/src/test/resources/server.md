| Field                      | Type     | Default   | Description |
|----------------------------|----------|-----------|-------------|
| binding-host               | Text     | `0.0.0.0` |             |
| binding-port               | Integer  | `8080`    |             |
| ssl                        |          |           |             |
| accept-continue            | Bool     | `false`   |             |
| keep-alive                 | Bool     | `true`    |             |
| request-decompression      |          | `no`      |             |
| response-compression       |          |           |             |
| request-streaming          |          |           |             |
| max-header-size            | Integer  | `8192`    |             |
| log-warning-on-fatal-error | Bool     | `true`    |             |
| graceful-shutdown-timeout  | Duration | `PT10S`   |             |
| idle-timeout               | Duration | `None`    |             |