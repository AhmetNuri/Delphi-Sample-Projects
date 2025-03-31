BEGIN TRANSACTION;
DROP TABLE IF EXISTS "ListViews";
CREATE TABLE IF NOT EXISTS "ListViews" (
	"ListViewID"	INTEGER,
	"ListViewName"	TEXT NOT NULL,
	PRIMARY KEY("ListViewID" AUTOINCREMENT)
);
DROP TABLE IF EXISTS "Headers";
CREATE TABLE IF NOT EXISTS "Headers" (
	"HeaderID"	INTEGER,
	"HeaderListViewID"	INTEGER NOT NULL,
	"HeaderTitle"	TEXT NOT NULL,
	"HeaderColor"	TEXT,
	"HeaderIndex"	INTEGER,
	"HeaderOrder"	INTEGER,
	"HeaderSVGData"	TEXT,
	PRIMARY KEY("HeaderID" AUTOINCREMENT),
	FOREIGN KEY("HeaderListViewID") REFERENCES "ListViews"("ListViewID")
);
DROP TABLE IF EXISTS "ComboBoxItems";
CREATE TABLE IF NOT EXISTS "ComboBoxItems" (
	"ComboBoxItemID"	INTEGER,
	"ComboBoxItemFieldID"	INTEGER NOT NULL,
	"ComboBoxItemValue"	TEXT NOT NULL,
	"ComboBoxItemOrder"	INTEGER,
	PRIMARY KEY("ComboBoxItemID" AUTOINCREMENT),
	FOREIGN KEY("ComboBoxItemFieldID") REFERENCES "Fields"("FieldID")
);
DROP TABLE IF EXISTS "Fields";
CREATE TABLE IF NOT EXISTS "Fields" (
	"FieldID"	INTEGER,
	"FieldHeaderID"	INTEGER NOT NULL,
	"FieldName"	TEXT NOT NULL,
	"FieldValue"	TEXT,
	"FieldDataType"	TEXT,
	"FieldOrder"	INTEGER,
	"FieldProperties"	TEXT DEFAULT '{}',
	"FieldType"	TEXT,
	"FieldLabel"	TEXT,
	PRIMARY KEY("FieldID" AUTOINCREMENT),
	FOREIGN KEY("FieldHeaderID") REFERENCES "Headers"("HeaderID")
);
DROP TABLE IF EXISTS "FieldParameters";
CREATE TABLE IF NOT EXISTS "FieldParameters" (
	"ParameterID"	INTEGER,
	"FieldID"	INTEGER NOT NULL,
	"ParameterName"	TEXT NOT NULL,
	"ParameterValue"	TEXT NOT NULL,
	PRIMARY KEY("ParameterID" AUTOINCREMENT),
	FOREIGN KEY("FieldID") REFERENCES "Fields"("FieldID") ON DELETE CASCADE
);
COMMIT;
