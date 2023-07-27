# JSON Schema To Elm

## What's this?

This is a tool to convert a JSON Schema document to working Elm code that decodes and encodes the schema types.

The use case for us was to generate JSON schema from a pydantic `BaseModel` and then read that data from Elm.

## Example

Given the following Python file using pydantic-1:

```python
class MyEnum(Enum):
    MEMBER1 = "first"
    MEMBER2 = "second"

class TestClass(BaseModel):
    my_int: int
    my_enum: MyEnum

print(TestClass.schema_json(indent=2))
```

The JSON schema output will be:

```json
{
  "title": "TestClass",
  "type": "object",
  "properties": {
    "myint": {
      "title": "Myint",
      "type": "integer"
    },
    "myenum": {
      "$ref": "#/definitions/MyEnum"
    }
  },
  "required": [
    "myint",
    "myenum"
  ],
  "definitions": {
    "MyEnum": {
      "title": "MyEnum",
      "description": "An enumeration.",
      "enum": [
          "first",
	  "second"
      ]
    }
  }
}
```

Then running our converter, we get the following Elm code (trimmed down):

```elm
type alias TestClass = { myint : Int, myenum : MyEnum }
type MyEnum = First | Second

decodeMyEnum : Decode.Decoder MyEnum
decodeMyEnum =
    Decode.oneOf [decodeConstantString "first" First, decodeConstantString "second" Second]


encodeMyEnum : MyEnum -> Encode.Value
encodeMyEnum v =
    case v of
        First ->
            Encode.string "first"
        Second ->
            Encode.string "second"

decodeTestClass : Decode.Decoder TestClass
decodeTestClass =
    (DecodePipeline.required "myenum" decodeMyEnum) ((DecodePipeline.required "myint" Decode.int) (Decode.succeed TestClass))
```

## Build/Install

### Using Nix

Easy enough:

```
nix develop
cabal run -- haskell-json-schema-to-elm MyElmModuleNamePleaseReplace < schema-document.json > MyElmModuleNamePleaseReplace.elm
```

# License

We adopted [language-elm](https://github.com/eliaslfox/language-elm#readme): Copyright Elias Lawson-Fox (c) 2017 (see https://hackage.haskell.org/package/language-elm).
