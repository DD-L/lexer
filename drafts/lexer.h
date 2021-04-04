//
// copyright (c) Deel/linan
// Home:      https://github.com/DD-L/lexer
// License:   MIT
// 
#pragma once

#include <cctype>
#include <cstring>
#include <utility>
#include <unordered_map>
#include <functional>
#include <vector>
#include <deque>
#include <assert.h>

#ifndef DDL_LEXER
#define DDL_LEXER DDL_LEXER
#endif // DDL_LEXER

namespace DDL_LEXER
{
    class Lexer;
   
    struct StrRef
    {
        const char* str = nullptr;
        std::size_t len = 0;

    public:
        constexpr StrRef() = default;

        constexpr StrRef(const char* _str, std::size_t _len)
            : str(_str), len(_len)
        {}

        template <std::size_t N>
        constexpr StrRef(const char (&arr)[N] )
            : str(arr), len(N - 1)
        {
            static_assert(0 != N, "Error");
        }

        constexpr StrRef(const StrRef& strRef) noexcept
            : str(strRef.str), len(strRef.len)
        {}

        std::string ToStdString() const
        {
            return std::string(str, len);
        }

        const char* operator+(std::size_t offset) const noexcept
        {
            return offset < len ? str + offset : nullptr;
        }

        char operator[](std::size_t offset) const noexcept
        {
            return *((*this) + offset); // may crash ...
        }
    }; //  StrRef


    class VariableAllocator;

    enum class VariableFlag
    {
        Normal,
        Mutable,
    };

    // Syntax Variable 
    class Variable
    {
    public:
        Variable() = default;
        explicit Variable(VariableFlag flag)
            : m_flag(flag)
        {}

        virtual ~Variable() {
            m_mut = nullptr;
            m_flag = VariableFlag::Normal;
        }


        virtual bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept
        {
            (void)script; (void)offset; (void)err;
            return true;
        }

        virtual bool Action() noexcept { return true; }; // = 0; @TODO

        bool IsMutable() const
        {
            return VariableFlag::Mutable == m_flag;
        }

    protected:
         void SetName(StrRef name) noexcept
         {
             m_varName = name;
         }

         const StrRef& Name() const
         {
             return m_varName;
         }

         void SwapMut(Variable* newMut, Variable*& oldMut = ThreadLocalDummyVariable()) noexcept
         {
             oldMut = m_mut;
             m_mut = newMut;
         }

         static Variable*& ThreadLocalDummyVariable() noexcept
         {
             thread_local Variable* dummy = nullptr;
             return dummy;
         }

        template <class Allocator>
        void ReshapeToMut(Allocator& allocator) noexcept;

        // 只需 内置 SyntaxVar 重写 _Move 即可
        virtual Variable* _Move(VariableAllocator& allocator) noexcept
        {
            return nullptr; (void)allocator;
        }

    protected:
        friend class Lexer;

        Variable*    m_mut = nullptr;  // 用作动态绑定，仅仅被用在 mut_var 的情况；
        // 但有必须写在基类中，原因是确保其他子类对象可以被安全的重塑为 MutableVariable 对象
        VariableFlag m_flag = VariableFlag::Normal;
        StrRef       m_varName;
    }; // class Variable

    namespace traits
    {
        //template <class Syntax>
        struct AllocHelper
        {
            template <class Syntax, class Vars>
            static Variable* Alloc(Vars&, Variable* newVar) noexcept
            {
                return newVar;
            }

            template <class Syntax, class Vars, class... Args>
            static Variable* Alloc(Vars& vars, Args&&... args) noexcept
            {
                Variable* var = new (std::nothrow) Syntax(std::forward<Args>(args)...);
                if (var)
                {
                    vars.push_back(var);
                }
                return var;
            }
        };
    } // namespace traits

    // 变量内存分配器，同时承担内存持有的职责
    class VariableAllocator
    {
    public:
        ~VariableAllocator() noexcept
        {
            Destroy();
        }

        template <class Syntax, class... Args>
        Variable* Alloc(Args&&... args) noexcept
        {
            typedef DDL_LEXER::traits::AllocHelper AllocHelper;
            return AllocHelper::Alloc<Syntax>(m_vars, std::forward<Args>(args)...);
        }

    private:
        void Destroy() noexcept
        {
            for (Variable* v : m_vars)
            {
                delete v;
            }

            m_vars.clear();
        }
    private:
        std::deque<Variable*>  m_vars;
    }; // class VariableAllocator

    namespace internal
    {
        //
        // 一种动态绑定的 Variable.
        // 其意义是尽可能的复用文法（运行期），或者前式声明（编译期）
        class MutableVariable : public Variable
        {
        public:
            explicit MutableVariable(Variable* mut = nullptr)
                : Variable(VariableFlag::Mutable)
            {
                Set(mut);
            }

            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // 确保其他 Variable 衍生对象可以安全的重塑为 mut_var
                static_assert(sizeof(MutableVariable) == sizeof(Variable), "Error"); 

                assert(Valid());
                return m_mut->Scan(script, offset, err);
            }

            bool Valid() const
            {
                return (nullptr != m_mut);
            }

            bool Set(Variable* newVar) noexcept
            {
                m_mut = newVar;
                return Valid();
            }
        }; // class MutableVariable

        // 终止符
        class SyntaxToken : public Variable
        {
            typedef SyntaxToken _Myt;

        public:
            SyntaxToken() = default;

            SyntaxToken(StrRef token)
                : m_token(token)
            {}

            [[nodiscard]] bool AddLeftWhites(Variable* whites) noexcept
            { // 要求：当前 Token 被当前设定的白字符从左到右 Scan，不能出现被消耗字符的情况
                std::size_t offset = 0;
                if (whites)
                { 
                    m_leftWhites = nullptr;

                    std::string e;
                    whites->Scan(m_token, offset, e); (void)e;
                }

                m_leftWhites = whites;
                return (0 == offset);
            }

            virtual bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept
            {
                if ((offset + m_token.len) > script.len)
                {
                    return false;
                }

                std::size_t oldOffset = offset;
                if (m_leftWhites)
                {
                    std::string e;
                    m_leftWhites->Scan(script, offset, e); (void)e;
                }

                if (ScanImpl(script, offset, err))
                {
                    return true;
                }
                offset = oldOffset; // 恢复前面白字符的消耗 ？？ @TODO 有必要 ？？？？
                return false;
            }
        
            virtual bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept
            {
                if (0 == std::memcmp(script + offset, m_token.str, m_token.len))
                {
                    offset += m_token.len;
                    return true;
                }
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxToken>(m_token);
                this->~SyntaxToken();
                return var;
            }
        private:
            StrRef    m_token;
            Variable* m_leftWhites  = nullptr;
            //Variable* m_rightWhites = nullptr;
        }; // class SyntaxToken

        // Basic Structures (syntax/sentence structure)
        class SyntaxSequence : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxSequence(Args&&... args)
                : m_sequence({ std::forward<Args>(args) ... })
            {}

            explicit SyntaxSequence(std::vector<Variable*>&& branch)
                : m_sequence(std::move(branch))
            {}

            void AppendVariable(Variable* var)
            {
                m_sequence.push_back(var);
            }

            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                std::size_t oldOffset = offset;
                std::size_t cnt = 0;
                for (Variable* v : m_sequence)
                {
                    if (!v->Scan(script, offset, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                    ++cnt;
                }

                if (m_sequence.size() == cnt)
                {
                    return true;
                }
                offset = oldOffset;
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxSequence>(std::move(m_sequence));
                this->~SyntaxSequence();
                return var;
            }

        protected:
            std::vector<Variable*>  m_sequence;
        }; // class SyntaxSequence

        // 有序分支结构
        class SyntaxBranch : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxBranch(Args&&... args)
                : m_branchs({ std::forward<Args>(args) ... })
            {}

            explicit SyntaxBranch(std::vector<Variable*>&& branch)
                : m_branchs(std::move(branch))
            {}

            void AppendVariable(Variable* var)
            {
                m_branchs.push_back(var);
            }

            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                for (Variable* v : m_branchs)
                {
                    if (v->Scan(script, offset, err))
                    {
                        return true;
                    }
                }

                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxBranch>(std::move(m_branchs));
                this->~SyntaxBranch();
                return var;
            }

        protected:
            std::vector<Variable*>  m_branchs;
        }; // class SyntaxBranch

        class SyntaxLoop : public Variable
        {
        public:
            enum : std::size_t
            {
                Max = ~std::size_t(0)
            };

        public:
            SyntaxLoop(Variable* var, std::size_t min, std::size_t max)
                : m_var(var), m_min(min), m_max(max)
            {}

            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                std::size_t oldOffset = offset;
                std::size_t cnt = 0;
                for (std::size_t i = 0; i < m_min; ++i, ++cnt)
                { // 最少循环 m_min 次
                    if (!m_var->Scan(script, offset, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                }

                for (std::size_t i = m_min; i < m_max; ++i, ++cnt)
                {
                    oldOffset = offset;
                    if (! m_var->Scan(script, offset, err))
                    {
                        offset = oldOffset;
                        break;
                    }
                }

                return ((m_min <= cnt) && (cnt <= m_max));
                //if ((m_min <= cnt) && (cnt <= m_max))
                //{
                //    return true;
                //}
                //offset = oldOffset;
                //return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                Variable* var = allocator.Alloc<SyntaxLoop>(m_var, m_min, m_max);
                this->~SyntaxLoop();
                return var;
            }

        protected:
            Variable*   m_var = nullptr;
            std::size_t m_min = 0;
            std::size_t m_max = 0;
        }; // class SyntaxLoop

        ///////////////////////////////////////////////////////////////


        // 内建标识符
        class BuiltinIdent : public SyntaxToken
        { // [_a-zA-Z][_a-zA-Z0-9]*
        public:
            using SyntaxToken::SyntaxToken;

        private:
            bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // [_a-z-A-Z][_0-9a-zA-Z]*
                // 最小要求是一个字符
                std::size_t curOffset = offset;
                if (curOffset >= script.len)
                {
                    err = Name().ToStdString() + ": atleast one bytes.....";
                    return false;
                }

                const char first = script[curOffset++];

                if (!(('_' == first) 
                    || (first >= 'a' && first <= 'z')
                    || (first >= 'A' && first <= 'Z')))
                {
                    return false;
                }

                for (; curOffset < script.len; ++curOffset)
                {
                    const char ch = script[curOffset];
                    if (!('_' == ch
                        || (ch >= 'a' && ch <= 'z')
                        || (ch >= 'A' && ch <= 'Z')
                        || (ch >= '0' && ch <= '9')))
                    {
                        //err = Name().ToStdString() + ": Error";
                        //return false;
                        break;
                    }
                }

                offset = curOffset;
                return true;
            }
        }; // class BuiltinIdent


        // 属性
        class BuiltinAttr : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                return true;
            }
        };

        class BuiltinWhite : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                if (offset < script.len)
                {
                    switch (script[offset])
                    {
                    case '\x09': // \t 
                    case '\x0a': // \n
                    case '\x0b': // \v
                    case '\x0c': // \f
                    case '\x0d': // \r
                    case '\x20': // ' '
                        ++offset;
                        return true;
                    }
                }
                return false;
            }
        }; // class BuiltinWhite

        //class BuildinWhites : public SyntaxLoop
        //{ // // 包含所有的空白字符 和 注释
        //public:
        //    BuildinWhites()
        //    {
        //    }
        //}; // class BuildinWhites

        class BuiltinNaturalNumDec : public SyntaxToken
        { // [1-9][0-9]*|0
        public:
            using SyntaxToken::SyntaxToken;

            bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            { // 十进制自然数
                std::size_t curOffset = offset;
                if (curOffset < script.len)
                {
                    if (IsZero(script[curOffset])) 
                    { 
                        ++curOffset; 
                        offset = curOffset; 
                        return true; 
                    }

                    if (Is1To9(script[curOffset]))
                    {
                        ++curOffset;

                        while (curOffset < script.len)
                        {
                            if (Is0To9(script[curOffset]))
                            {
                                ++curOffset;
                            }
                            else
                            {
                                break;
                            }
                        }
                        offset = curOffset;
                        return true;
                    }
                }
                return false;
            }

            bool IsZero(char c) const { return '0' == c; }
            bool Is1To9(char c) const { return '1' <= c && c <= '9'; }
            bool Is0To9(char c) const { return IsZero(c) || Is1To9(c); }
        }; // class BuiltinNaturalNumDec

        // 内置终结符的表达方式：单引号或双引号
        class BuiltinTerminator : public SyntaxToken
        {
        public:
            using SyntaxToken::SyntaxToken;

            // 1. 单引号不支持转义，2. 双引号支持转义
            bool ScanImpl(const StrRef& script, std::size_t& offset, std::string& err) noexcept override
            {
                // 1. 单引号： '[^']+
                // 2. 双引号： 暂不实现

                std::size_t curOffset = offset;
                if (curOffset < script.len)
                {
                    if ('\'' == script[curOffset])
                    {
                        ++curOffset;
                        while (curOffset < script.len)
                        {
                            if ('\'' == script[curOffset++])
                            {
                                offset = curOffset;
                                return true;
                            }
                        }
                    }
                }
                return false;
            }
        };
    } // namespace internal

    class Lexer
    {
    public:
        typedef std::unordered_map<std::string, Variable*> VarsTable;

    public:
        Lexer()
        {
            MakeInternal();
        }

        VariableAllocator& GetVariableAllocator()
        {
            return m_variableAllocator;
        }

        // 线程无关
        bool SetGrammar(StrRef script, const VarsTable& varsTable, std::string& err) const noexcept
        {
            err.clear();
            std::size_t offset = 0;
            return ScanScript(script, varsTable, offset, err);
        }

        const Variable* GetVariable(const std::string& varName) noexcept
        {
            auto found = m_userVariablesMap.find(varName);
            if (m_userVariablesMap.end() != found)
            {
                return found->second;
            }
            return nullptr;
        }

    private:
        //static std::size_t LocateToRoot(const StrRef& script, const StrRef& rootStr) noexcept
        //{
        //    if (rootStr.len > script.len || 0 == rootStr.len)
        //    {
        //        return 0;
        //    }
        //
        //    // .*?;?\s*$root[\s:]
        //    // State: Accept
        //    // 0. Start
        //    // 1. ;?
        //    // 2. \s*
        //    // 3. $root
        //    // 4. [\s:]
        //    // 5. End
        //
        //    SyntaxToken _0x3B(";");
        //    SyntaxLoop  _0x3BOption(&_0x3B, 0, 1u);
        //
        //    internal::BuiltinWhite _white;
        //    SyntaxLoop _whitesOption(&_white, 0, SyntaxLoop::Max);
        //     
        //    SyntaxToken _rootStr(rootStr);
        //    SyntaxBranch _last(&_white, &_0x3B);
        //
        //    SyntaxSequence state(&_0x3BOption, &_whitesOption, &_rootStr, &_last);
        //
        //    const std::size_t end = script.len;
        //    std::size_t offset    = 0;
        //
        //    std::string err;
        //    while (offset < script.len)
        //    {
        //        if (state.Scan(script, offset, err))
        //        {
        //            assert(offset > 1);
        //            return (offset - 1u >= rootStr.len) ? (offset - 1u - rootStr.len) : end;
        //        }
        //    } 
        //
        //    return end;
        //}

        bool ScanScript(const StrRef& script, const VarsTable&, 
            std::size_t offset, std::string& err) const noexcept
        {
            std::vector<StrRef> tokenStream; // TokenStream

            if (m_internalRoot->Scan(script, offset, err))
            {
                return script.len == offset;
            }

            return false;
        }

    private:
        template <class T, class... Args,
           class = typename std::enable_if<!std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* Alloc(StrRef name, Args&&...args) noexcept
        {
            Variable* ret = m_variableAllocator.Alloc<T>(std::forward<Args>(args)...);
            if (nullptr != ret)
            {
                ret->SetName(name);
            }
            return ret;
        }

        template <class T, 
            class = typename std::enable_if<std::is_base_of<Variable, T>::value>::type>
        Variable* Alloc(Variable* var) noexcept
        {
            return m_variableAllocator.Alloc<T>(var);
        }

        template <class T, 
            class = typename std::enable_if<std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* Alloc(StrRef name, StrRef token, Variable* whites = nullptr) noexcept
        {
            internal::SyntaxToken* ret 
                = (internal::SyntaxToken*)m_variableAllocator.Alloc<T>(token);
            if (nullptr != ret)
            {
                ret->SetName(name);
                if (! ret->AddLeftWhites(whites))
                {
                    return nullptr;
                }
            }
            return ret;
        }

        //template <class T, class Var, class... Args>
        //Variable* PlacementAlloc(Var* var, Args&&...args) noexcept
        //{
        //    var.~Var();
        //    return new (var) T(std::forward<Args>(args)...);
        //}

        // internal: 内置语法
        bool MakeInternal() noexcept
        {
            using namespace internal;
            Variable* whites = Alloc<SyntaxLoop>("whites", 
                Alloc<BuiltinWhite>("white"), 0, SyntaxLoop::Max);

            // ident      : ; # func  (暂时使用函数)
            Variable* ident = Alloc<BuiltinIdent>("ident", "", whites);
            // head       :  ident ;
            Variable* head = Alloc<SyntaxSequence>(ident);
            // var        : ident ;
            Variable* var  = Alloc<SyntaxSequence>(ident);

            // terminator    : '.*?'  # 这里暂时只使用单引号的终结符 (内部 CFG 文法中终结符的表示)
            Variable* terminator = Alloc<BuiltinTerminator>("terminator", "", whites);

            // operand 
            Variable* operand = Alloc<SyntaxBranch>("operand", terminator, var);

            // expr : 优先级 循环 >  分支 > 序列

            Variable* $0x7B = Alloc<SyntaxToken>("{", "{", whites); // 可复用
            Variable* $0x7D = Alloc<SyntaxToken>("}", "}", whites); // 可复用
            Variable* $0x2C = Alloc<SyntaxToken>(",", ",", whites); // 可复用

            // decl expr;
            Variable* expr = Alloc<MutableVariable>("expr"); // mut_var

            // struct_with_bracket : '(' expr ')' ;
            Variable* struct_with_bracket = Alloc<SyntaxSequence>("struct_with_bracket",
                Alloc<SyntaxToken>("(", "(", whites), expr, Alloc<SyntaxToken>(")", ")", whites));

            // struct_expr : operand | struct_with_bracket # 这里 仅能容纳 var ??? 那正则表达式怎么办 ？？？TODO,  可以使用变量！！！！
            Variable* struct_expr = Alloc<SyntaxBranch>("struct_expr", operand, struct_with_bracket);

            // num_dec regex : [1-9][0-9]*|0 ;
            Variable* num_dec = Alloc<BuiltinNaturalNumDec>("num_dec", "", whites);

            Variable* loop_n = Alloc<SyntaxSequence>("loop_n", $0x7B, num_dec, $0x7D);
            // loop_m_n   : '{' num_dec ',' num_dec '}';
            Variable* loop_m_n = Alloc<SyntaxSequence>("loop_m_n", 
                $0x7B, num_dec, $0x2C, num_dec, $0x7D);
            // loop_m_max : '{' num_dec ',' '}';
            Variable* loop_m_max = Alloc<SyntaxSequence>("loop_m_max", $0x7B, num_dec, $0x2C, $0x7D);

            // loop_symbol     : '?' | '*' | "+" | loop_n | loop_m_n | loop_m_max ;
            Variable* loop_symbol = Alloc<SyntaxBranch>("loop_symbol",
                Alloc<SyntaxToken>("?", "?", whites), 
                Alloc<SyntaxToken>("*", "*", whites), 
                Alloc<SyntaxToken>("+", "+", whites),
                loop_n, loop_m_n, loop_m_max);
            // loop_symbol_opt : loop_symbol ?
            Variable* loop_symbol_opt = Alloc<SyntaxLoop>("loop_symbol_opt", loop_symbol, 0, 1u);
            // loop_expr : struct_expr loop_symbol_opt;
            Variable* loop_expr = Alloc<SyntaxSequence>("loop_expr", struct_expr, loop_symbol_opt);

            // seq_expr : loop_expr +
            Variable* seq_expr = Alloc<SyntaxLoop>("seq_expr", loop_expr, 1u, SyntaxLoop::Max);

            // branch_expr : '|' seq_expr;
            Variable* branch_expr = Alloc<SyntaxSequence>("branch_expr", Alloc<SyntaxToken>("|", "|", whites), seq_expr);
            // seq_expr_some : branch_expr *;
            Variable* branch_expr_some = Alloc<SyntaxLoop>("branch_expr_some", branch_expr, 0, SyntaxLoop::Max);
            // branch_pairs   : branch_expr branch_expr_some
            Variable* branch_pairs = Alloc<SyntaxSequence>("branch_pairs", seq_expr, branch_expr_some);

            // expr      ： branch_expr;
            assert(expr->IsMutable());
            expr->SwapMut(branch_pairs);

            // defaultBody: ':' expr;
            Variable* defaultBody = Alloc<SyntaxSequence>("defaultBody", Alloc<SyntaxToken>(":", ":", whites), expr);
            
            // attr      : ; # func  # func: 'regex' | 其他自定义白字符策略变量
            Variable* attr = Alloc<BuiltinAttr>("attr");

            // attrBody:    attr defaultBody;
            Variable* attrBody  = Alloc<SyntaxSequence>("attrBody", attr, defaultBody);

            // body: defaultBody | attrBody;
            Variable* body  = Alloc<SyntaxBranch>("body", defaultBody, attrBody);

            // production :  head body ';' ; 
            Variable* production = Alloc<SyntaxSequence>("production", head, body, Alloc<SyntaxToken>(";", ";", whites), whites);

            // root:         production + ;
            m_internalRoot = Alloc<SyntaxLoop>("root", production, 1u, SyntaxLoop::Max);
            return true;
        }

    private:
        Variable*                                  m_internalRoot;
        Variable*                                  m_builtinWhites;  // 内置白字符
        VariableAllocator                          m_variableAllocator;
        std::unordered_map<std::string, Variable*> m_userVariablesMap;
    }; // class Lexer


    /*
char_1_9 : '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
char_0_9 : '0' | char_1_9 ;

num  :  char_1_9 char_0_9* | '0' ;


loop_suffix_n_m   : '{' num ',' num '}' ;
loop_suffix_n     : '{' num '}'         ;
loop_suffix_n_max : '{' num ',' '}'     ;

loop_suffix : '?' | '*' | '+' | loop_suffix_n_m | loop_suffix_n  | loop_suffix_n_max ;


decl  branch;

expr   : ident | ( branch )  ;

loop   :  expr loop_suffix ? ; 
seq    :  loop +             ;
branch_vec : ( '|' seq )*    ; 
branch :  seq  branch_vec    ;
    
    */

    //////////////////////////////////////////////
    // Impl

    template <class Allocator>
    void Variable::ReshapeToMut(Allocator& allocator) noexcept
    {
        if (!IsMutable())
        {
            Variable* var = _Move(allocator);

            new (this) internal::MutableVariable();
            this->m_mut = var;
        }
    }

} // namespace DDL_LEXER