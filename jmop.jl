macro defclass(name, superclasses, slots)
    begin
        println("Executing @defclass macro with arguments: ")
        println("Name of the class: $(string(name))")
        println("Superclasses: $(string(superclasses))")
        println("Slots: $(string(slots))")
        _supers = eval(superclasses)
        _slots = eval(slots)
        quote
            struct $(length(_supers) > 0 ? Expr(:(<:), :name, Union{_supers...}) : esc(name))
                $(for slot in _slots println(slot) end)
            end
        end
    end
end