Tags:

-   [Note Taking](20241112132027.md)
-   [How I do stuff](20250209192043.md)


# How I do knowledge management / note taking

I take notes using the [Zettelkasten Method](20221208132210.md) with [Org Roam](20240330111506.md) as my tool.

    @startuml 
    abstract Note {
        + id 
        + title
    }
    
    entity AtomicNote <<Note>> {
        + tags
        + references
    }
    
    entity SourceMaterialNote <<Note>> {
    }
    
    entity TagNote <<Note>> {
    }
    
    entity StructureNote <<Note>> {
    }
    
    StructureNote --> AtomicNote
    AtomicNote --> SourceMaterialNote: references
    AtomicNote --> TagNote: tags
    
    @enduml

