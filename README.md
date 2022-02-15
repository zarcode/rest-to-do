# Todo List REST API in Haskell

## Description
REST API with basic CRUD actions on todo list

## How to run localy?
Download or clone master branch. In the root project directory run:

```
stack build --exec to-do-app-exe
```

Server will run on:

```
http://localhost:8080/
```
and you are ready to "hit" some endpoints.

Here are some curl examples:

1. Get whole todo list: 
```
    curl --location --request GET 'http://localhost:8080/todos'
```

2. Add a new item to the list: 
```
    curl --location --request POST 'http://localhost:8080/todos' \
    --header 'Content-Type: application/json' \
    --data-raw '{
        "dueBy": "2010/10/21 12:00:00",
        "priority": "Normal",
        "title": "Change16",
        "description": "description"
    }'
```
3. Get single list item by id: 
```
    curl --location --request GET 'http://localhost:8080/todo/4'
```

4. Edit an existing item to the list: 
```
    curl --location --request POST 'http://localhost:8080/todo/4' \
    --header 'Content-Type: application/json' \
    --data-raw '{
        "mbTitle": "Change17"
    }'
```

5. Delete single list item by id:
```
    curl --location --request DELETE 'http://localhost:8080/todo/6'
```

