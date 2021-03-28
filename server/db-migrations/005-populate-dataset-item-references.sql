insert into dataset (name) values ('weight');
insert into dataset_field (dataset_id, field_name) select id, 'weight' from dataset where name = 'weight';
update data_item i set dataset_id = (select id from dataset where name = i.dataset);
