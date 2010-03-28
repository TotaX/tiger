structure tigerenv =
struct
    datatype EnvEntry = 
        VarEntry of {   access : tigertranslate.access,
                        level : int,
                        ty : tigertypes.Ty}
        | FuncEntry of {level : tigertranslate.level,
                        label : string,
                        formals : tigertypes.Ty list,
                        result : tigertypes.Ty}
end
