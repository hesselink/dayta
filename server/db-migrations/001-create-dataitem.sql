create table data_item
( id serial primary key
, username varchar(256) not null
, dataset varchar(256) not null
, datetime timestamp with time zone not null
, values jsonb not null
)
