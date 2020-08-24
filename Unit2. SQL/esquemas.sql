begin;
create function rbt(a bigint,b bigint) returns bigint as $$ SELECT (floor(random()*(b-a))+a)::bigint; $$ language sql;
create table ocupacion(idOcupacion serial primary key, nombre text not null unique);
create or replace function randocup() returns int as $$ select rbt(1,(select last_value from ocupacion_idocupacion_seq))::int; $$ language sql;
create table estado (idEstado serial primary key, claveEstado char(2) not null unique,nombre text not null unique, poblacion bigint not null);
create table municipio (idMunicipio bigserial primary key, idEstado bigint not null references estado(idEstado), nombre text not null);
create table asentamiento (idAsentamiento bigserial primary key, idMunicipio bigint not null references municipio(idMunicipio), cp int not null, nombre text not null, unique (nombre,cp));
create or replace function randas() returns bigint as $$ select rbt(1,144655); $$ language sql;
create table domicilio (idDomicilio bigserial primary key,
                        calle text not null,
                        numex text not null,
                        numin text,
                        idAsentamiento bigint references asentamiento(idAsentamiento) default randas(),
                        unique (calle,numex,numin,idAsentamiento));
create or replace function randdom() returns bigint as $$ select rbt(107435,(select 1+last_value from domicilio_iddomicilio_seq)); $$ language sql;

create table cliente (
       idcliente bigserial primary key,
       saldo float not null,
       descuento float not null,
       tipo char not null,--e,i
       límitecrédito float not null);

create table persona (
             idPersona bigserial primary key,
             nombre text not null,
             ap1 text not null,
             ap2 text,
             sexo char(1) not null,
             fnac date not null,
             idDomicilio bigint not null references domicilio(idDomicilio) default randdom(),
             idOcupacion int not null references ocupacion(idOcupacion) default randocup(),
             idCliente bigint references cliente(idCliente),
             unique (nombre,ap1,ap2,sexo,fnac));

create table empresa (
       idempresa bigserial primary key,
       rfc varchar(13) not null,
       rasoc text not null,
       nombreCom text not null,
       giro text not null,
       iddomicilio bigint not null references domicilio(iddomicilio),
       idCliente bigint references cliente(idcliente)
       unique (rasoc,nombrecom));

create table fabrica (
       idFabrica bigserial primary key,
       totalArts int not null,
       idEmpresa bigint not null references empresa(idempresa));

create table articulo (
       idarticulo bigserial primary key,
       descripcion text not null unique,
       precio float,
       idFabrica bigint references fabrica(idfabrica));

create table pedido (
       idPedido bigserial primary key,
       fecha timestamp without time zone not null default now(),
       total float not null default 0,
       arts int not null default 0,
       nombreTienda text not null,
       idCliente bigint not null references cliente(idcliente));

create table artspedido (
       idPedido bigserial not null references pedido(idpedido),
       idArticulo bigserial not null references articulo(idarticulo),
       cantidad int not null,
       precioventa float not null);
       
commit;
