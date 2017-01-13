create procedure dbo.pCloneDataBase
(	@szDBName sysname
,	@szDBNameTarget sysname
)
as
begin

select * from sys.databases where name = @szDBName

end