CREATE PROCEDURE [dbo].[pTableClone] (@TableName sysname
, @IncludeConstraints bit = 1
, @IncludeIndexes bit = 1
, @NewTableName sysname = NULL
, @UseSystemDataTypes bit = 0)
AS
BEGIN
  DECLARE @MainDefinition TABLE (
    FieldValue varchar(200)
  )

  DECLARE @DBName sysname
  DECLARE @ClusteredPK bit
  DECLARE @TableSchema nvarchar(255)
  SET @DBName = DB_NAME(DB_ID())
  
  SELECT
    @TableName = name
  FROM sysobjects
  WHERE id = OBJECT_ID(@TableName)
  
  DECLARE @ShowFields TABLE (
    FieldID int IDENTITY (1, 1),
    DatabaseName varchar(100),
    TableOwner varchar(100),
    TableName varchar(100),
    FieldName varchar(100),
    ColumnPosition int,
    ColumnDefaultValue varchar(100),
    ColumnDefaultName varchar(100),
    IsNullable bit,
    DataType varchar(100),
    MaxLength int,
    NumericPrecision int,
    NumericScale int,
    DomainName varchar(100),
    FieldListingName varchar(110),
    FieldDefinition char(1),
    IdentityColumn bit,
    IdentitySeed int,
    IdentityIncrement int,
    IsCharColumn bit
  )

  DECLARE @HoldingArea TABLE (
    FldID smallint IDENTITY (1, 1),
    Flds varchar(4000),
    FldValue char(1) DEFAULT (0)
  )
  
  DECLARE @PKObjectID TABLE (
    ObjectID int
  )
  
  DECLARE @Uniques TABLE (
    ObjectID int
  )
  
  DECLARE @HoldingAreaValues TABLE (
    FldID smallint IDENTITY (1, 1),
    Flds varchar(4000),
    FldValue char(1) DEFAULT (0)
  )
  
  DECLARE @Definition TABLE (
    DefinitionID smallint IDENTITY (1, 1),
    FieldValue varchar(200)
  )
  
  INSERT INTO @ShowFields (DatabaseName, TableOwner, TableName, FieldName, ColumnPosition, ColumnDefaultValue, ColumnDefaultName, IsNullable, DataType, MaxLength, NumericPrecision, NumericScale, DomainName, FieldListingName, FieldDefinition, IdentityColumn, IdentitySeed, IdentityIncrement, IsCharColumn)
    SELECT
      DB_NAME(),
      TABLE_SCHEMA,
      TABLE_NAME,
      COLUMN_NAME,
      CAST(ORDINAL_POSITION AS int),
      COLUMN_DEFAULT,
      dobj.name AS ColumnDefaultName,
      CASE
        WHEN c.IS_NULLABLE = 'YES' THEN 1
        ELSE 0
      END,
      DATA_TYPE,
      CAST(CHARACTER_MAXIMUM_LENGTH AS int),
      CAST(NUMERIC_PRECISION AS int),
      CAST(NUMERIC_SCALE AS int),
      DOMAIN_NAME,
      COLUMN_NAME + ',',
      '' AS FieldDefinition,
      CASE
        WHEN ic.object_id IS NULL THEN 0
        ELSE 1
      END AS IdentityColumn,
      CAST(ISNULL(ic.seed_value, 0) AS int) AS IdentitySeed,
      CAST(ISNULL(ic.increment_value, 0) AS int) AS IdentityIncrement,
      CASE
        WHEN st.collation_name IS NOT NULL THEN 1
        ELSE 0
      END AS IsCharColumn
    FROM INFORMATION_SCHEMA.COLUMNS c
    JOIN sys.columns sc ON c.TABLE_NAME = OBJECT_NAME(sc.object_id) AND c.COLUMN_NAME = sc.Name
    LEFT JOIN sys.identity_columns ic ON c.TABLE_NAME = OBJECT_NAME(ic.object_id) AND c.COLUMN_NAME = ic.Name
    JOIN sys.types st  ON COALESCE(c.DOMAIN_NAME, c.DATA_TYPE) = st.name
    LEFT OUTER JOIN sys.objects dobj  ON dobj.object_id = sc.default_object_id AND dobj.type = 'D'
    WHERE c.TABLE_NAME = @TableName
    ORDER BY c.TABLE_NAME, c.ORDINAL_POSITION

  SELECT TOP 1
    @TableSchema = TableOwner
  FROM @ShowFields

  INSERT INTO @HoldingArea (Flds)
    VALUES ('(')

  INSERT INTO @Definition (FieldValue)
    VALUES ('CREATE TABLE ' + CASE WHEN @NewTableName IS NOT NULL THEN @NewTableName ELSE @DBName + '.' + @TableSchema + '.' + @TableName END)

  INSERT INTO @Definition (FieldValue)
    VALUES ('(')

  INSERT INTO @Definition (FieldValue)
    SELECT
      CHAR(10) + FieldName + ' ' + CASE
        WHEN DomainName IS NOT NULL AND
          @UseSystemDataTypes = 0 THEN DomainName + CASE
            WHEN IsNullable = 1 THEN ' NULL '
            ELSE ' NOT NULL '
          END
        ELSE UPPER(DataType) + CASE
            WHEN IsCharColumn = 1 THEN '(' + CASE
                WHEN MaxLength > 0 THEN CAST(MaxLength AS varchar(10))
                ELSE 'Max'
              END + ')'
            ELSE ''
          END + CASE
            WHEN IdentityColumn = 1 THEN ' IDENTITY(' + CAST(IdentitySeed AS varchar(5)) + ',' + CAST(IdentityIncrement AS varchar(5)) + ')'
            ELSE ''
          END + CASE
            WHEN IsNullable = 1 THEN ' NULL '
            ELSE ' NOT NULL '
          END + CASE
            WHEN ColumnDefaultName IS NOT NULL AND
              @IncludeConstraints = 1 THEN 'CONSTRAINT [' + ColumnDefaultName + '] DEFAULT' + UPPER(ColumnDefaultValue)
            ELSE ''
          END
      END + CASE
        WHEN FieldID = (SELECT
            MAX(FieldID)
          FROM @ShowFields) THEN ''
        ELSE ','
      END
    FROM @ShowFields
  
  IF @IncludeConstraints = 1
  BEGIN
    INSERT INTO @Definition (FieldValue)
      SELECT
        ',CONSTRAINT [' + name + '] FOREIGN KEY (' + ParentColumns + ') REFERENCES [' + ReferencedObject + '](' + ReferencedColumns + ')'
      FROM (SELECT
        ReferencedObject = OBJECT_NAME(fk.referenced_object_id),
        ParentObject = OBJECT_NAME(parent_object_id),
        fk.name,
        REVERSE(SUBSTRING(REVERSE((SELECT
          cp.name + ','
        FROM sys.foreign_key_columns fkc
        JOIN sys.columns cp ON fkc.parent_object_id = cp.object_id AND fkc.parent_column_id = cp.column_id
        WHERE fkc.constraint_object_id = fk.object_id
        FOR xml PATH (''))
        ), 2, 8000)) ParentColumns,
        REVERSE(SUBSTRING(REVERSE((SELECT
          cr.name + ','
        FROM sys.foreign_key_columns fkc
        JOIN sys.columns cr ON fkc.referenced_object_id = cr.object_id  AND fkc.referenced_column_id = cr.column_id
        WHERE fkc.constraint_object_id = fk.object_id
        FOR xml PATH (''))
        ), 2, 8000)) ReferencedColumns
      FROM sys.foreign_keys fk) a
      WHERE ParentObject = @TableName
  
    INSERT INTO @Definition (FieldValue)
      SELECT
        ',CONSTRAINT [' + name + '] CHECK ' + definition
      FROM sys.check_constraints
      WHERE OBJECT_NAME(parent_object_id) = @TableName

    INSERT INTO @PKObjectID (ObjectID)
      SELECT DISTINCT
        PKObject = cco.object_id
      FROM sys.key_constraints cco
      JOIN sys.index_columns cc ON cco.parent_object_id = cc.object_id AND cco.unique_index_id = cc.index_id
      JOIN sys.indexes i ON cc.object_id = i.object_id AND cc.index_id = i.index_id
      WHERE OBJECT_NAME(parent_object_id) = @TableName AND i.type = 1 AND is_primary_key = 1

    INSERT INTO @Uniques (ObjectID)
      SELECT DISTINCT
        PKObject = cco.object_id
      FROM sys.key_constraints cco
      JOIN sys.index_columns cc ON cco.parent_object_id = cc.object_id  AND cco.unique_index_id = cc.index_id
      JOIN sys.indexes i ON cc.object_id = i.object_id AND cc.index_id = i.index_id
      WHERE OBJECT_NAME(parent_object_id) = @TableName
      AND i.type = 2
      AND is_primary_key = 0
      AND is_unique_constraint = 1

    SET @ClusteredPK =
                      CASE
                        WHEN @@ROWCOUNT > 0 THEN 1
                        ELSE 0
                      END
    INSERT INTO @Definition (FieldValue)
      SELECT
        ',CONSTRAINT ' + name + CASE type
          WHEN 'PK' THEN ' PRIMARY KEY ' + CASE
              WHEN pk.ObjectID IS NULL THEN ' NONCLUSTERED '
              ELSE ' CLUSTERED '
            END
          WHEN 'UQ' THEN ' UNIQUE '
        END + CASE
          WHEN u.ObjectID IS NOT NULL THEN ' NONCLUSTERED '
          ELSE ''
        END + '(' + REVERSE(SUBSTRING(REVERSE((SELECT
          c.name + +CASE
            WHEN cc.is_descending_key = 1 THEN ' DESC'
            ELSE ' ASC'
          END + ','
        FROM sys.key_constraints ccok
        LEFT JOIN sys.index_columns cc ON ccok.parent_object_id = cc.object_id AND cco.unique_index_id = cc.index_id
        LEFT JOIN sys.columns c ON cc.object_id = c.object_id AND cc.column_id = c.column_id
        LEFT JOIN sys.indexes i ON cc.object_id = i.object_id AND cc.index_id = i.index_id
        WHERE i.object_id = ccok.parent_object_id AND ccok.object_id = cco.object_id
        FOR xml PATH (''))
        ), 2, 8000)) + ')'
      FROM sys.key_constraints cco
      LEFT JOIN @PKObjectID pk ON cco.object_id = pk.ObjectID
      LEFT JOIN @Uniques u ON cco.object_id = u.objectID
      WHERE OBJECT_NAME(cco.parent_object_id) = @TableName
  END

  INSERT INTO @Definition (FieldValue)
    VALUES (')')
  
  IF @IncludeIndexes = 1
  BEGIN
    INSERT INTO @Definition (FieldValue)
      SELECT
        'CREATE ' + CASE is_unique
          WHEN 1 THEN 'UNIQUE '
          ELSE ''
        END + type_desc + ' INDEX [' + [name] COLLATE SQL_Latin1_General_CP1_CI_AS + '] ON [' + OBJECT_NAME(object_id) + '] (' + REVERSE(SUBSTRING(REVERSE((SELECT
          name + CASE
            WHEN sc.is_descending_key = 1 THEN ' DESC'
            ELSE ' ASC'
          END + ','
        FROM sys.index_columns sc
        JOIN sys.columns c ON sc.object_id = c.object_id AND sc.column_id = c.column_id
        WHERE OBJECT_NAME(sc.object_id) = @TableName
        AND sc.object_id = i.object_id AND sc.index_id = i.index_id
        ORDER BY index_column_id ASC
        FOR xml PATH (''))
        ), 2, 8000)) + ')'
      FROM sys.indexes i
      WHERE OBJECT_NAME(object_id) = @TableName
      AND
         CASE
           WHEN @ClusteredPK = 1 AND
             is_primary_key = 1 AND
             type = 1 THEN 0
           ELSE 1
         END = 1
      AND is_unique_constraint = 0
      AND is_primary_key = 0
  END

  INSERT INTO @MainDefinition (FieldValue)
    SELECT
      FieldValue
    FROM @Definition
    ORDER BY DefinitionID ASC
  
  SELECT
    *
  FROM @MainDefinition

END