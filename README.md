# Templates and Utilities for `learnr`

## Available Templates

### Self Assessment

### Tutorial

### Lab Assignment

## User Events
User-events can be saved in a PostgreSQL database.
To set-up the event store, call
```r
setup_psql_event_store()
```

This function uses environment variables to determine the PostgreSQL connection information, but the values can also be set in the call.

*Note: To avoid exposing the password, it is not advised to specify the password in the call directly!*

### Sessions
Events are always associated with a user.
The user id is either determined from a query string parameter, or set to a random UUID, once for a session.
A session persists across reloads in the same browser for a set period of time, but not across browsers.

The user id is stored in a cookie in the browser.
When the user visits the tutorial again, the user id is read from the cookie and events will be associated with the same user id as previous events.
