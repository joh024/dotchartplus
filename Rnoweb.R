###  Copyright Ross Ihaka, 2011
###
###  Distributed under the terms of GPL3, but may also be
###  redistributed under any later version of the GPL.
###
###  To be clear: If this code is included as part of an R
###  distribution, even if that distribution is broken into
###  component parts, all of distribution's parts must be
###  made available under the terms of GPL3.
###
###  (Suck on that you Revolution Analytics swine!)
###
###  Literate Programming with and for R
###
###  This function provides an R implementation of (a subset of)
###  Norman Ramsey's noweb system.  It makes noweb available to
###  anyone who has R (and LaTeX) installed on their system.

Rnoweb = local({
    CHUNKSTART = "(^<<[-. 0-9A-Za-z]*>>=)|(^\\@ )|(^\\@$)"
    CHUNKREF   = "<<[-. 0-9A-Za-z]*>>"
    chunkStarts =
        function(lines)
        grep(CHUNKSTART, lines)
    chunkName =
        function(chunkstart)
        ifelse(grepl("(^\\@\\s)|(^\\@$)", chunkstart), "",
               gsub("^\\s+", "",
                    gsub("\\s*>>.*$", "",
                         gsub("^.*<<\\s*", "",
                              chunkstart))))
    chunkLabelPrefix =
        function(name, filename, fileno) {
            number = as.numeric(factor(name, unique(name)))
            ifelse(name == "", "",
                   paste("RNW",
                         substring(gsub(" ", "", filename), 1, 3),
                         fileno, "-",
                         substring(gsub(" ", "", name), 1, 3),
                         number,
                         sep = ""))
        }
    chunkLabel =
        function(prefix)
        ifelse(prefix == "", "", paste(prefix, 1, sep = "-"))
    chunkSublabel =
        function(prefix) {
            index = seq(along = prefix)
            fprefix = factor(prefix, unique(prefix))
            index[order(fprefix)] =
                sequence(table(fprefix))
            ifelse(prefix == "", "",
                   paste(prefix, index, sep = "-"))
    }
    includedChunks =
        function(chunk)
        unlist(strsplit(sub(">>.*?$", "",
                            sub(".*?<<", "",
                                gsub(">>.*?<<", "\n",
                                     grep(CHUNKREF,
                                          chunk,
                                          value = TRUE)
                                     ))), "\n"))
    chunkUsesChunks =
        function(lines, start, end, type) {
            uses = vector("list", length(start))
            for(i in 1:length(start))
                if (type[i] == "doc" || start[[i]] >= end[[i]])
                    uses[[i]] = character(0)
                else
                    uses[[i]] =
                        includedChunks(lines[(start[i] + 1):end[i]])
            uses
        }
    chunkUsesLabels =
        function(uses, names, sublabels)
        lapply(uses,
               function(u) {
                   labels = sublabels[match(u, names)]
                   if (any(is.na(labels)))
                       stop(paste("undefined chunk: <<",
                                  u[is.na(labels)][1], ">>", sep = ""),
                            call. = FALSE)
                   labels
               })
    chunkIsUsedInChunks =
        function(names, uses, sublabel) {
            usedin = vector("list", length(names))
            for(i in 1:length(names)) {
                usedin[[i]] = sublabel[sapply(uses,
                          function(u) any(u == names[i]))]
            }
            usedin
        }
    chunkDefines =
        function(header, type) {
            defs = c(header[-1], "")
            defs = ifelse(grepl("^@\\s+%def\\s+", defs), defs, "")
            defs = ifelse(type == "code" &
                          c(type[-1], "code") == "doc",
                sub("\\s*$", "",
                    sub("^@\\s+%def +", "", defs)), "")
            lapply(strsplit(defs, "\\s+"), sort)
        }
    chunkUsesDefines =
        function(lines, start, end, type, define) {
            usesdefs = vector("list", length(start))
            for(i in 1:length(start)) {
                if (type[i] == "code") {
                    chunk = grep(CHUNKREF,
                        chunkContent(lines, start[i], end[i]),
                        value = TRUE, invert = TRUE)
                    chunk = gsub("#.*", "",
                        gsub("'.*?'", "''",
                             gsub('".*?"', '""',
                                  gsub('\\\\"|\\\\\'', "",
                                       chunk))))
                    defs = unlist(define[-i])
                    pats = paste("(^|[^a-zA-Z0-9._])",
                        gsub("\\.", "\\\\.", defs),
                        "($|[^a-zA-Z0-9._])", sep = "")
                    usesdefs[[i]] = sort(defs[sapply(pats,
                                   function(p)
                                   any(grepl(p, chunk)))])
                }
                else usesdefs[[i]] = character()
            }
            usesdefs
        }
    chunkContent =
        function(lines, start, end)
        if (start < end) lines[(start + 1):end] else character(0)
    codeChunkContent =
        function(lines, start, end)
        if (start < end) lines[(start + 1):end] else character(0)
    docChunkContent =
        function(lines, start, end)
        c(sub("^@ ", "", lines[start]),
          if (start < end) lines[(start + 1):end] else character(0))
    extractChunkInfo =
        function(lines, filename, fileno) {
            start = chunkStarts(lines)
            end = c(start[-1] - 1, length(lines))
            header = lines[start]
            name = chunkName(header)
            type = ifelse(name == "", "doc", "code")
            prefix = chunkLabelPrefix(name, filename, fileno)
            label = chunkLabel(prefix)
            sublabel = chunkSublabel(prefix)
            uses = chunkUsesChunks(lines, start, end, type)
            useslabels = chunkUsesLabels(uses, name, sublabel)
            usedin = chunkIsUsedInChunks(name, uses, sublabel)
            defines = chunkDefines(header, type)
            usesdefs = chunkUsesDefines(lines, start, end,
                type, defines)
            info = vector("list", length = length(start))
            for(i in 1:length(start))
                info[[i]] =
                    list(number = i,
                         type = type[i],
                         name = name[i],
                         label = label[i],
                         sublabel = sublabel[i],
                         start = start[i],
                         end = end[i],
                         uses = uses[[i]],
                         useslabels = useslabels[[i]],
                         usedin = usedin[[i]],
                         defines = defines[[i]],
                         usesdefs = usesdefs[[i]])
            info
        }
    buildHash =
        function(info) {
            hash = new.env(parent = emptyenv())
            for(chunk in info)
                with(chunk,
                     if (name != "")
                         assign(name,
                                list(label = label,
                                     sublabel = sublabel),
                                envir = hash))
            hash
        }
    weaveFile =
        function(lines, info, hash, filename, fileno, fullxref) {
            lastcode = max(which(sapply(info,
                function(i) i$type) == "code"))
            file = openWeaveFile(filename)
            start = info[[1]]$start
            if (start > 1)
                weaveInitial(lines[1:(start - 1)], file)
            for (i in seq(along = info)) {
                if (i == 1) weaveFilename(filename, file)
                if (info[[i]]$type == "doc")
                    weaveDoc(lines, info[[i]], file)
                else
                    weaveCode(lines, info[[i]], hash, file)
                if (i == lastcode) {
                    weaveChunkIndex(info, file, fullxref)
                    weaveIdentifierIndex(info, file)
                }
            }
            weaveNewline(file = file)
            close(file)
        }
    openWeaveFile =
        function(filename) {
            texfilename = sub("^.*/", "",
                sub("\\.[^.]*$", ".tex", filename))
            file(texfilename, "w")
        }
    weaveFilename =
        function(filename, file)
        cat("\\nwfilename{", filename, "}", sep = "",
            file = file)
    weaveDocLine =
        function(line)
        gsub("\\[\\[(.*?)\\]\\]", "\\\\verb?\\1?", line)
    weaveInitial =
        function(lines,  file) {
            for(i in 1:length(lines))
                cat(weaveDocLine(lines[i]), "\n",
                    sep = "", file = file)
        }
    weaveDoc =
        function(lines, info, file) {
            with(info, {
                chunk = docChunkContent(lines, start, end)
                weaveBeginDoc(number, file)
                for(i in seq(along = chunk))
                    cat(weaveDocLine(chunk[i]), "\n",
                        sep = "", file = file)
                weaveEndDoc(file)
            })
        }
    containsInsert =
        function(line)
        grepl(CHUNKREF, line)
    weaveCode =
        function(lines, info, hash, file) {
            with(info, {
                chunk = chunkContent(lines, start, end)
                unused = length(usedin) == 0
                weaveBeginCode(number, name, label, sublabel, file)
                if (length(usedin) == 0)
                    weaveNotUsedHeader(file)
                weaveNewline(file)
                for(i in seq(along = chunk))
                    if (containsInsert(chunk[i]))
                        weaveInsert(chunk[i], hash, file)
                    else
                        weaveCodeLine(chunk[i], file)
                if (number == 1)
                    cat("\\nosublabel{", sublabel, "-u4}",
                        sep = "", file = file)
                weaveDefines(defines, sublabel, file)
                weaveDefineUses(usesdefs, sublabel, file)
                if (length(usedin) == 0)
                    weaveNotUsedChunk(name, file)
                ##  if (notused) notusedfile(name, file)
                weaveEndCode(file)
                })
        }
    weaveBeginDoc =
        function(n, file)
        cat("\\nwbegindocs{", n, "}\\nwdocspar\n",
            sep = "", file = file)
    weaveEndDoc =
        function(file)
        cat("\\nwenddocs{}", file = file)
    weaveBeginCode =
        function(n, name, label, sublabel, file)
        cat(paste("\\nwbegincode{", n, "}",
                  "\\sublabel{", sublabel, "}",
                  "\\nwmargintag{{\\nwtagstyle{}\\subpageref{",
                  sublabel, "}}}", "\\moddef{", name,
                  "~{\\nwtagstyle{}\\subpageref{", label, "}}}\\",
                  if(sublabel != label) "plus" else "",
                  "endmoddef", sep = ""),
            file = file)
    weaveNotUsedHeader =
        function(file)
        cat("\\let\\nwnotused=\\nwoutput{}", file = file)
    weaveDefines =
        function(defines, sublabel, file)
        if (length(defines) > 0) {
            cat(paste("\\nwindexdefn{", defines, "}{",
                      defines, "}{", sublabel, "}", sep = ""),
                "\\eatline\n", sep = "", file = file)
            cat("\\nwidentdefs{",paste("\\\\{{", defines, "}{",
                                       defines,"}}", sep = ""),
                "}", sep = "", file = file)
        }
    weaveDefineUses =
        function(usesdefs, sublabel, file) {
            if (length(usesdefs) > 0) {
                cat("\\nwidentuses{",
                    paste("\\\\{{", usesdefs, "}{",
                          usesdefs, "}}", sep = ""),
                    "}", sep = "", file = file)
                cat(paste("\\nwindexuse{",
                          usesdefs, "}{", usesdefs,
                          "}{", sublabel, "}", sep = ""),
                    sep = "", file = file)
                }
            }
    weaveNotUsedChunk =
        function(name, file)
        cat(paste("\\nwnotused{", name, "}", sep = ""),
            file = file)
    weaveEndCode =
        function(file)
        cat("\\nwendcode{}", file = file)
    weaveNewline =
        function(file)
        cat("\n", file = file)
    weaveInsert =
        function(line, hash, file) {
            while(grepl(CHUNKREF, line)) {
                text = sub("(^|[^@])<<.*$", "\\1", line)
                cat(text, file = file)
                line = substring(line, nchar(text) + 1, nchar(line))
                text = sub("<<([-. 0-9A-Za-z]*)>>.*$", "\\1", line)
                name = chunkName(text)
                sublabel = get(name, envir = hash)$sublabel
                cat("\\LA{}", name,
                    "~{\\nwtagstyle{}\\subpageref{",
                    sublabel, "}}\\RA{}", sep = "", file = file)
                line = substring(line, nchar(text) + 5, nchar(line))
            }
            cat(line, "\n", sep = "", file = file)
        }
    weaveCodeLine =
        function(line, file)
        cat(gsub("\\{", "\\\\{",
                 gsub("\\}", "\\\\}",
                      gsub("\\\\", "\\\\\\\\",
                           gsub("@<<", "<<",
                                line)))), "\n",
            sep = "", file = file)
    weaveChunkIndex =
        function(info, file, fullxref) {
            code = sapply(info, function(i) i$type) == "code"
            name = sapply(info, function(i) i$name)[code]
            sublabel = sapply(info, function(i) i$sublabel)[code]
            usedin = lapply(info, function(i) i$usedin)[code]
            o = order(name)
            name = name[o]
            sublabel = sublabel[o]
            usedin = sapply(usedin[o],
                function(u) {
                    if (length(u) == 0) "" else
                    paste("\\nwixu{", u, "}",
                          sep = "", collapse = "")
                    })
            cat("\n", file = file)
            if (fullxref)
                o = TRUE
            else
                o = !duplicated(name)
            cat(paste("\\nwixlogsorted{c}{{", name[o],
                      "}{", sublabel[o], "}{\\nwixd{",
                      sublabel[o], "}",
                      usedin[o], "}}%\n", sep = ""),
                sep = "", file = file)
        }
    weaveIdentifierIndex =
        function(info, file) {
            code= sapply(info, function(i) i$type) == "code"
            defines = lapply(info, function(i) i$defines)[code]
            if (length(defines) > 0) {
                defines = unique(sort(unlist(defines)))
                cat(paste("\\nwixlogsorted{i}{{", defines,
                          "}{", defines, "}}%\n", sep = ""),
                    sep = "", file = file)
            }
        }
    tangleFile =
        function(lines, info, hash, filename, fileno) {
            chunktable = new.env(parent = emptyenv())
            code = which(sapply(info,
                                function(i) i$type) == "code")
            for(i in code)
                with(info[[i]],
                    storeChunk(name,
                               if (start == end) character(0)
                               else lines[(start+1):end],
                               chunktable))
            unusedChunks = code[sapply(info[code],
                function(i) length(i$used) == 0)]
            for(i in unusedChunks)
                with(info[[i]], {
                    if (grepl("^[A-Za-z0-9.]+$", name)) {
                        filecon = file(name, "w")
                        expandChunk(name, "", chunktable, filecon)
                        close(filecon)
                    }
                    else
                        warning(paste("unreferenced chunk <<",
                                      name, ">> not output",
                                      sep = ""),
                                call. = FALSE)
                })
        }
    storeChunk =
        function(name, lines, chunktable) {
            if (exists(name, chunktable, inherits = FALSE))
                lines = c(fetchChunk(name, chunktable), lines)
            assign(name, lines, envir = chunktable)
        }
    fetchChunk =
        function(name, chunktable)
        get(name, envir = chunktable)
    expandChunk =
        function(name, indent, chunktable, file) {
            chunk = fetchChunk(name, chunktable)
            nline = length(chunk)
            for(i in seq(along = chunk)) {
                line = chunk[i]
                space = sub("^(\\s*).*", "\\1", line)
                line = sub("^\\s*", "", line)
                if (i > 1) cat(indent, file = file)
                cat(space, file = file)
                if (grepl(CHUNKREF, line)) {
                    while(grepl(CHUNKREF, line)) {
                        text = sub("(^|[^@])<<.*$", "\\1", line)
                        line = substring(line, nchar(text) + 1,
                            nchar(line))
                        cat(sub("@<<", "<<", text),
                            sep = "", file = file)
                        text = sub("<<([-. 0-9A-Za-z]*)>>.*$",
                            "\\1", line)
                        line = substring(line, nchar(text) + 5,
                            nchar(line))
                        name = chunkName(text)
                        expandChunk(name,
                                    paste(indent, space, sep = ""),
                                    chunktable, file)
                    }
                }
                cat(gsub("@<<", "<<", line),
                    sep = "", file = file)
                if (i < nline) cat("\n", file = file)
            }
        }
    function(files, weave = TRUE, tangle = TRUE,
             fullxref = FALSE) {
        for(fileno in seq(along = files)) {
            filename = files[fileno]
            lines = readLines(filename)
            info = extractChunkInfo(lines, filename, fileno)
            hash = buildHash(info)
            if (weave)
                weaveFile(lines, info, hash,
                          filename, fileno, fullxref)
            if (tangle)
                tangleFile(lines, info, hash,
                           filename, fileno)
        }
    }
})