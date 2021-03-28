create table dataset_field
( id serial primary key
, dataset_id bigint not null references dataset(id)
, field_name text not null
)
