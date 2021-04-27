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
#include <string>
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

        template <class Iter>
        constexpr StrRef(Iter&& begin, Iter&& end)
            : str(&*begin), len((std::size_t)(end - begin))
        {
            assert(begin <= end);
        }

        template <std::size_t N>
        constexpr StrRef(const char (&arr)[N] )
            : str(arr), len(N - 1)
        {
            static_assert(0 != N, "Error");
        }

        constexpr StrRef(const StrRef& strRef) noexcept
            : str(strRef.str), len(strRef.len)
        {}

        bool IsNull() const
        {
            assert((nullptr == str) ? (0 == len) : true);
            return nullptr == str;
        }

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

    class VariableType
    {
    public:
        bool IsNormal() const
        {
            return !IsMutable();
        }

        void SetNormal()
        {
            m_flag = WipeTop();
        }

        bool IsMutable() const
        {
            return 0 != TopBit();
        }

        void SetMutable()
        {
            m_flag |= (~(Classify::classMask));
        }

        bool IsLoop() const
        {
            return  Classify::_Loop == WipeTop();
        }

        void SetLoop()
        {
            m_flag =  KeepTop() | Classify::_Loop;
        }

        bool IsBranch() const
        {
            return Classify::_Branch == WipeTop();
        }

        void SetBranch()
        {
            m_flag = KeepTop() | Classify::_Branch;
        }

        bool IsSequence() const
        {
            return Classify::_Sequence == WipeTop();
        }

        void SetSequence()
        {
            m_flag = KeepTop() | Classify::_Sequence;
        }

        bool IsTerminator() const
        {
            return Classify::_Terminator == WipeTop();
        }

        void SetTerminator()
        {
            m_flag = KeepTop() | Classify::_Terminator;
        }

        bool IsDollarFunc() const
        {
            return Classify::_DollarFunc == WipeTop();
        }

        void SetDollarFunc()
        {
            m_flag = KeepTop() | Classify::_DollarFunc;
        }

    private:
        uint8_t WipeTop() const
        {
            return (m_flag & Classify::classMask);
        }

        uint8_t KeepTop() const
        {
            return  m_flag & (~(Classify::classMask));
        }

        uint8_t TopBit() const
        {
            return KeepTop() >> 7;
        }

        enum Classify : uint8_t
        {
            _Branch     = 1,
            _Sequence   = 2,
            _Loop       = 3,
            _Terminator = 4,
            _DollarFunc = 5,

            classMask   = 0x7f,
        }; // enum Flag

        uint8_t  m_flag = 0;
    }; // class VariableType

    class Food
    {
    public:
        Food() = default;

        Food(const char* begin, const char* end, 
            VariableType type, std::size_t constraint, uint64_t ctxInt)
            : m_begin(begin), m_end(end), m_constraint(constraint), m_type(type)
        {
            assert(begin <= end);
        }

        const char* Begin() const
        {
            return m_begin;
        }

        const char* End() const
        {
            return m_end;
        }

        std::size_t Length() const
        {
            assert(Begin() <= End());
            return End() - Begin();
        }
        
        StrRef ToStrRef() const
        {
            return { Begin(), End() };
        }

        std::size_t BranchID() const
        {
            assert(m_type.IsBranch());
            return m_branchID;
        }

        std::size_t LoopCount() const
        {
            assert(m_type.IsLoop());
            return m_loopCnt;
        }

        void* Data() const
        {
            return m_data;
        }

        static uint64_t AsInt(void* data) noexcept
        {
            return reinterpret_cast<uint64_t>(data);
        }

    protected:
        const char* m_begin = nullptr;
        const char* m_end   = nullptr;

        union
        {
            std::size_t m_constraint = 0;
            std::size_t m_branchID;
            std::size_t m_seqSize;
            std::size_t m_loopCnt;
        };

        union
        {
            uint64_t m_ctxInt = 0;
            void*    m_data;
        };

        VariableType    m_type;

    }; // class Food

    class Action
    {
    public:
        virtual ~Action() {}
        virtual bool Handler(const Food& food, void* ctx, std::string&) = 0;
    }; // class Action

    struct ActionQueueEle
    {
        Action* action = nullptr;
        Food    food;

        ActionQueueEle() = default;
        ActionQueueEle(Action* _ac, Food&& _food) : action(_ac), food(std::move(_food)) {}
    }; // struct ActionQueueEle

    typedef std::vector<ActionQueueEle> ActionQueue;

    // Syntax Variable 
    class Variable
    {
    public:
        Variable() = default;
        //explicit Variable(VariableType type)
        //    : m_type(type)
        //{}
        //
        virtual ~Variable() {
            m_mut = nullptr;
            m_type = VariableType();
        }

        virtual bool Scan(const StrRef& script, std::size_t& offset,
            ActionQueue& actions, std::string& err) noexcept = 0;
        //{
        //    (void)script; (void)offset; (void)actions, (void)err;
        //    return true; // 空串
        //}

        bool IsMutable() const
        {
            return m_type.IsMutable();
        }

    public:
        void SetAction(Action* action)
        {
            m_action = action;
        }

        Action* GetAction() const
        {
            return m_action;
        }

        const VariableType& Type() const
        {
            return m_type;
        }

    public:
         void _SetName(StrRef name) noexcept
         {
             m_varName = name;
         }

         const StrRef& _Name() const
         {
             return m_varName;
         }

         void _SwapMut(Variable* newMut, Variable*& oldMut = ThreadLocalDummyVariable()) noexcept
         {
             oldMut = m_mut;
             m_mut = newMut;
         }

    protected:
         void PushAction(ActionQueue& actions, const StrRef& script,
             std::size_t start, std::size_t curr, std::size_t constraint, uint64_t data) const noexcept
         {
             if (m_action)
             {
                 actions.emplace_back(m_action, 
                     Food(script + start, script + curr, m_type, constraint, data));
             }
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

        Variable*            m_mut = nullptr;  // 用作动态绑定，仅仅被用在 mut_var 的情况；
        // 但有必须写在基类中，原因是确保其他子类对象可以被安全的重塑为 MutableVariable 对象
        VariableType         m_type;
        StrRef               m_varName;
        DDL_LEXER::Action*   m_action = nullptr;
    }; // class Variable

    namespace internal
    {
        class MutableVariable;
    } // internal

    // 变量内存分配器，同时承担内存持有的职责
    class VariableAllocator
    {
    public:
        ~VariableAllocator() noexcept
        {
            Destroy();
        }

        template <class Syntax, class... Args>
        Syntax* Alloc(Args&&... args) noexcept
        {
            return ForceAlloc<Syntax>(std::forward<Args>(args)...);
        }

        template <class Syntax,
            class = typename std::enable_if<!std::is_same<Syntax, internal::MutableVariable>::value>::type>
        Variable* Alloc(Variable* v) noexcept
        {
            return v;
        }

        template <class Syntax, class... Args>
        Syntax* ForceAlloc(Args&&... args)
        {
            Syntax* var = new (std::nothrow) Syntax(std::forward<Args>(args)...);
            if (nullptr != var)
            {
                m_vars.push_back(var);
            }
            return var;
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

    // 字符串字面值内存分配器
    class MemoryAllocator final
    {
    public:
        char* Alloc(std::size_t n)
        {
            char* ret = new char[n];
            m_holder.emplace_back(ret, n);
            return ret;
        }

        ~MemoryAllocator()
        {
            for (StrRef& ele : m_holder)
            {
                delete[] ele.str;
            }
            m_holder.clear();
        }

    private:
        std::vector<StrRef> m_holder;

    };

    namespace
    {
        struct RangeArg { constexpr RangeArg() = default; };
        constexpr RangeArg _range_arg;
    }

    namespace internal
    {
        //
        // 一种动态绑定的 Variable.
        // 其意义是尽可能的复用文法（运行期），或者前式声明（编译期）
        class MutableVariable : public Variable
        {
        public:
            explicit MutableVariable(Variable* mut = nullptr)
            {
                m_type.SetMutable();
                Set(mut);
            }

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                // 确保其他 Variable 衍生对象可以安全的重塑为 mut_var
                static_assert(sizeof(MutableVariable) == sizeof(Variable), "Error"); 

                assert(Valid());
                const std::size_t start = offset;
                if (m_mut->Scan(script, offset, actions, err))
                {
                    PushAction(actions, script, start, offset, 0, 0);
                    return true;
                }

                return false;
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

        // 终止符, 支持空串
        class SyntaxToken : public Variable
        {
            typedef SyntaxToken _Myt;

        public:
            SyntaxToken() { m_type.SetTerminator(); }
            explicit SyntaxToken(StrRef token) : m_token(token) { m_type.SetTerminator(); }

            [[nodiscard]] bool SetLeftWhites(Variable* whites) noexcept
            { // 要求：当前 Token 被当前设定的白字符从左到右 Scan，不能出现被消耗字符的情况
                std::size_t offset = 0;
                if (whites)
                { 
                    m_leftWhites = nullptr;

                    std::string e;
                    ActionQueue aq;
                    whites->Scan(m_token, offset, aq, e); (void)aq, (void)e;
                }

                m_leftWhites = whites;
                return (0 == offset);
            }

            bool SetRightAssert(Variable* _Assert) noexcept
            {
                m_rightAssert = _Assert;
                return true;
            }

            virtual bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept
            {
                std::size_t oldOffset = offset;
                if (m_leftWhites)
                {
                    std::string e;
                    ActionQueue aq;
                    m_leftWhites->Scan(script, offset, aq, e); (void)aq, (void)e;
                }

                uint64_t data = 0;
                const std::size_t start = offset;
                if (ScanImpl(script, offset, data, err))
                {
                    if (nullptr == m_rightAssert)
                    {
                        PushAction(actions, script, start, offset, 0, data);
                        return true;
                    }
                    // 对 Token 右边界的零宽断言
                    std::size_t constOffset = offset;
                    std::string e; 
                    ActionQueue aq;
                    (void)(constOffset); (void)(e); (void)aq;
                    if (m_rightAssert->Scan(script, constOffset, aq, e))
                    {
                        PushAction(actions, script, start, offset, 0, data); 
                        return true;
                    }
                }
                offset = oldOffset; // 恢复前面白字符的消耗 ？？ @TODO 有必要 ？？？？
                return false;
            }
        
            virtual bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t& data, std::string& err) noexcept
            {
                if ((offset + m_token.len) <= script.len)
                {
                    if (0 == std::memcmp(script + offset, m_token.str, m_token.len))
                    {
                        offset += m_token.len;
                        return true;
                    }
                }
                (void)data;
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxToken* var = allocator.ForceAlloc<SyntaxToken>(m_token);
                this->~SyntaxToken();
                return var;
            }
        private:
            StrRef    m_token;
            Variable* m_leftWhites   = nullptr;
            Variable* m_rightAssert  = nullptr;
        }; // class SyntaxToken

        // Basic Structures (syntax/sentence structure)
        class SyntaxSequence : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxSequence(Args&&... args)
                : m_sequence({ std::forward<Args>(args) ... })
            {
                m_type.SetSequence();
            }

            explicit SyntaxSequence(std::vector<Variable*>&& branch)
                : m_sequence(std::move(branch))
            {
                m_type.SetSequence();
            }

            template <class Iter>
            SyntaxSequence(RangeArg, Iter&& begin, Iter&& end)
                : m_sequence(begin, end)
            {
                m_type.SetSequence();
            }

            SyntaxSequence& AppendVariable(Variable* var)
            {
                m_sequence.push_back(var);
                return *this;
            }

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                const std::size_t start = offset;
                std::size_t cnt = 0;
                for (Variable* v : m_sequence)
                {
                    if (!v->Scan(script, offset, actions, err))
                    {
                        offset = start;
                        return false;
                    }
                    ++cnt;
                }

                if (m_sequence.size() == cnt)
                {
                    PushAction(actions, script, start, offset, m_sequence.size(), 0);
                    return true;
                }

                offset = start;
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxSequence* var = allocator.ForceAlloc<SyntaxSequence>(std::move(m_sequence));
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
                : m_branches({ std::forward<Args>(args) ... })
            {
                m_type.SetBranch();
            }

            template <class Iter>
            SyntaxBranch(RangeArg, Iter&& begin, Iter&& end)
                : m_branches(begin, end)
            {
                m_type.SetBranch();
            }

            explicit SyntaxBranch(std::vector<Variable*>&& branch)
                : m_branches(std::move(branch))
            {
                m_type.SetBranch();
            }

            void AppendVariable(Variable* var)
            {
                m_branches.push_back(var);
            }

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                for (std::size_t index = 0; index < m_branches.size(); ++index)
                {
                    const std::size_t oldQueSize = actions.size();
                    std::size_t start = offset;

                    Variable* v = m_branches[index];
                    if (v->Scan(script, offset, actions, err))
                    {
                        PushAction(actions, script, start, offset, index, 0);
                        return true;
                    }
                    else
                    {
                        actions.resize(oldQueSize);
                    }
                }

                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxBranch* var = allocator.ForceAlloc<SyntaxBranch>(std::move(m_branches));
                this->~SyntaxBranch();
                return var;
            }

        protected:
            std::vector<Variable*>  m_branches;
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
            {
                m_type.SetLoop();
            }

            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue& actions, std::string& err) noexcept override
            {
                std::size_t oldOffset = offset;
                const std::size_t start = offset;
                std::size_t cnt = 0;
                for (std::size_t i = 0; i < m_min; ++i, ++cnt)
                { // 最少循环 m_min 次
                    if (!m_var->Scan(script, offset, actions, err))
                    {
                        offset = oldOffset;
                        return false;
                    }
                }

                for (std::size_t i = m_min; i < m_max; ++i, ++cnt)
                {
                    oldOffset = offset;
                    if (! m_var->Scan(script, offset, actions, err))
                    {
                        offset = oldOffset;
                        break;
                    }
                }

                if ((m_min <= cnt) && (cnt <= m_max))
                {
                    PushAction(actions, script, start, offset, cnt, 0);
                    return true;
                }
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxLoop* var = allocator.ForceAlloc<SyntaxLoop>(m_var, m_min, m_max);
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
            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string& err) noexcept override
            {
                // [_a-z-A-Z][_0-9a-zA-Z]*
                // 最小要求是一个字符
                std::size_t curOffset = offset;
                if (curOffset >= script.len)
                {
                    err = _Name().ToStdString() + ": atleast one bytes.....";
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
                        //err = _Name().ToStdString() + ": Error";
                        //return false;
                        break;
                    }
                }

                offset = curOffset;
                return true;
            }
        }; // class BuiltinIdent

        class BuiltinWhite : public Variable
        {
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept override
            {
                if (offset < script.len && IsWhite(script[offset]))
                {
                    ++offset;
                    return true;
                }
                return false;
            }

        public:
            static bool IsWhite(char c) noexcept
            { // \t \n \v \f \r ' '
                switch (c)
                {
                case '\x09': case '\x0a': case '\x0b':
                case '\x0c': case '\x0d': case '\x20':
                    return true;
                default:
                    return false;
                }
            }
        }; // class BuiltinWhite

        class BuildinComment : public Variable
        { // 注释, 目前仅支持 #.*?(\n|$)
        public:
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string&) noexcept override
            {
                if (offset < script.len)
                {
                    if ('#' == script[offset])
                    {
                        ++offset;
                        while (offset < script.len)
                        {
                            if ('\n' == script[offset])
                            {
                                ++offset;
                                return true;
                            }
                            ++offset;
                        }

                        return true;
                    }
                }
                return false;
            }
        }; // class BuildinComment

        class BuiltinPunct : public Variable
        { // ispunct : !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~ 
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept override
            {
                if (offset < script.len)
                {
                    return IsPunct(script[offset]);
                }

                return false;
            }
        public:
            static bool IsPunct(char c) noexcept
            {
                return !!std::ispunct((uint8_t)(c));
            }
        }; // class BuiltinPunct

        class BuiltinTokenRightZeroWidthAssertion : public Variable
        { // Token 右边界的零宽断言
            // $|\s|[\x21–\x2F\x3A–\x40\x5B–\x60\x7B–\x7E]
            bool Scan(const StrRef& script, std::size_t& offset, ActionQueue&, std::string& err) noexcept override
            {
                const std::size_t _offset = offset; // 不消耗字符
                if (_offset == script.len)
                {
                    return true;
                }

                if (_offset < script.len)
                {
                    char c = script[_offset];
                    return BuiltinWhite::IsWhite(c) || BuiltinPunct::IsPunct(c);
                }

                return false;
            }
        };

        class BuiltinNaturalNumDec : public SyntaxToken
        { // [1-9][0-9]*|0
        public:
            using SyntaxToken::SyntaxToken;

            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t& data, std::string& err) noexcept override
            { // 十进制自然数
                std::size_t curOffset = offset;
                if (curOffset < script.len)
                {
                    data = 0;

                    if (IsZero(script[curOffset])) 
                    { 
                        ++curOffset; 
                        offset = curOffset; 
                        return true; 
                    }

                    if (Is1To9(script[curOffset]))
                    {
                        data = data * 10 + (script[curOffset] - '0');
                        ++curOffset;

                        while (curOffset < script.len)
                        {
                            if (Is0To9(script[curOffset]))
                            {
                                data = data * 10 + (script[curOffset] - '0');
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

        // 内置终结符的表达方式：单引号，双引号和//串
        class BuiltinTerminator : public SyntaxToken
        {
        public:
            using SyntaxToken::SyntaxToken;

            // 1. 单引号不支持转义，2. 双引号支持转义 3. 支持正则表达式
            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string& err) noexcept override
            {
                // 1. 单引号：      '[^']+
                // 2. 双引号：      暂不实现
                // 3. 正则表达式：  /.../ 支持空串
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
                    else if ('/' == script[curOffset])
                    { // 正则表达式， 不能出现不打印字符
                        ++curOffset;
                        while (curOffset < script.len)
                        {
                            if (! std::isprint((uint8_t)(script[curOffset])))
                            {
                                err = "Unprintable characters cannot appear in regex"; // offset 也要带出去
                                return false;
                            }
                            
                            if ('\\' == script[curOffset])
                            { // 转义符跳过
                                ++curOffset;
                            }
                            if ('/' == script[curOffset++])
                            {
                                offset = curOffset;
                                return true;
                            }
                        } // end while (curOffset < script.len)
                    } // end if '/'
                }
                return false;
            }
        }; // class BuiltinTerminator
       
        //////////////////////////////////
        // BulitinActions:
#undef _BULITIN_ACTIONS_DEFINE_BEGIN
#undef _BULITIN_ACTIONS_DEFINE_END
#define _BULITIN_ACTIONS_DEFINE_BEGIN(class_name) class class_name : public Action { 
#define _BULITIN_ACTIONS_DEFINE_END(class_name)   }

        template <class Vec>
        static inline void PureVec(Vec& vec) noexcept
        {
            vec.clear();
            vec.shrink_to_fit();
        }

        struct Context
        {
            void ResetStack() noexcept
            {
                //PureVec(m_stackHead);
                //PureVec(m_stackExpr);

#pragma warning("PureVec")
            }

            struct HeadEle
            {
                StrRef      rawRef;
                std::string strData;  // 低于 C++17 不支持透明Hash 也不支持 std::string_view
            
                HeadEle() = default;
                HeadEle(const StrRef& r, std::string&& s) : rawRef(r), strData(std::move(s)) {}
            }; // struct HeadEle

            std::vector<SyntaxToken*>                        m_stackTerminator;
            std::vector<StrRef>                              m_stackIdent;
            std::vector<HeadEle>                             m_stackHead;
            std::vector<Variable*>                           m_stackVar;
            std::vector<Variable*>                           m_stackOperand;
            std::vector<Variable*>                           m_stackStructExpr;
            std::vector<Variable*>                           m_stackExpr;
            std::vector<uint64_t>                            m_stackNumDec;
            std::vector<std::pair<std::size_t, std::size_t>> m_stackLoopSymbol;
            std::vector<std::size_t>                         m_stackLoopSymbolOpt;
            std::vector<Variable*>                           m_stackLoopExpr;
            std::vector<Variable*>                           m_stackSeqExpr;
            std::vector<std::size_t>                         m_stackBranchExprSome;

            std::unordered_map<std::string, Variable*>&     m_varMap;     
            VariableAllocator&                              m_varAlloc;
            MemoryAllocator&                                m_memAlloc;
            const std::unordered_map<std::string, Action*>& m_varActionMap;
        }; // Context

        static inline Context* Ctx(void* ctx) noexcept
        {
            assert(nullptr != ctx);
            return static_cast<Context*>(ctx);
        }

        /// <summary>
        ///  BuiltinTerminatorAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinTerminatorAc)
        bool Handler(const Food& food, void* ctx, std::string&) override
        { // Terminator 分三种： ''_   ""_  //_
            assert(food.Length() >= 2u);
            switch (*(food.Begin()))
            {
            case '\'':
                {
                    // 注意这里可能会存在 后缀！！！!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    SyntaxToken* token = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxToken>(StrRef(food.Begin() + 1, food.End() - 1));
                    Ctx(ctx)->m_stackTerminator.push_back(token); // 没有名字
                }
                break;
            case '"':
                {
                    // @TODO ！！！ 注意这里要转义 ，而且还有后缀问题 ！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
                    std::vector<char> aa(food.Begin() + 1, food.End() - 1);
                    char* buf = Ctx(ctx)->m_memAlloc.Alloc(aa.size()); 
                    std::memmove(buf, aa.data(), aa.size());
                    
                    SyntaxToken* token = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxToken>(StrRef(buf, aa.size()));
                    Ctx(ctx)->m_stackTerminator.push_back(token); // 没有名字
                }
                break;
            case '/':
                // @TODO 这里也有后缀问题！！！！

                break;
            default:
                return false;  // Error LogicBug
            }
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinTerminatorAc);

        /// <summary>
        ///   BuiltinIdentAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinIdentAc)
        bool Handler(const Food& food, void* ctx, std::string&) override
        {
            Ctx(ctx)->m_stackIdent.emplace_back(food.Begin(), food.End()); // StrRef 类型
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinIdentAc);
        
        /// <summary>
        ///  BuiltinHeadAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinHeadAc)
        bool Handler(const Food&, void* ctx, std::string& err) noexcept override
        {
            StrRef headRef = Ctx(ctx)->m_stackIdent.back();
            Ctx(ctx)->m_stackIdent.pop_back();

            std::string head = headRef.ToStdString();
            auto found = Ctx(ctx)->m_varMap.find(head);
            if (Ctx(ctx)->m_varMap.end() == found  || found->second->IsMutable())
            {
                Ctx(ctx)->m_stackHead.emplace_back(headRef, std::move(head));
                return true;
            }

            err = "Variable '" + head + "' : Duplicate definition";
            return false;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinHeadAc);
        
        /// <summary>
        /// BuiltinVarAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinVarAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::string ident = Ctx(ctx)->m_stackIdent.back().ToStdString();
            Ctx(ctx)->m_stackIdent.pop_back();

            auto found = Ctx(ctx)->m_varMap.find(ident);
            if (Ctx(ctx)->m_varMap.end() != found)
            {
                Ctx(ctx)->m_stackVar.push_back(found->second);
            }
            else
            { // 当前变量表还不存在， 先创建一个 mut_var
                Variable* mut = Ctx(ctx)->m_varAlloc.ForceAlloc<MutableVariable>();
                if (nullptr == mut)
                { // Error
                    return false;
                }
                Ctx(ctx)->m_varMap.emplace(ident, mut);
            }

            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinVarAc);

        /// <summary>
        ///  BuiltinOperandAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinOperandAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        { // terminator | var
            switch (food.BranchID())
            {
            case 0:
                Ctx(ctx)->m_stackOperand.push_back(Ctx(ctx)->m_stackTerminator.back());
                Ctx(ctx)->m_stackTerminator.pop_back();
                return true;
            case 1:
                Ctx(ctx)->m_stackOperand.push_back(Ctx(ctx)->m_stackVar.back());
                Ctx(ctx)->m_stackVar.pop_back();
                return true;
            default:
                return false;
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinOperandAc);

        /// <summary>
        ///  BuiltinStructExprAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinStructExprAc);
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        { // operand | struct_with_bracket 
            switch (food.BranchID())
            {
            case 0:
                Ctx(ctx)->m_stackStructExpr.push_back(Ctx(ctx)->m_stackOperand.back());
                Ctx(ctx)->m_stackOperand.pop_back();
                return true;
            case 1:
                Ctx(ctx)->m_stackStructExpr.push_back(Ctx(ctx)->m_stackExpr.back());
                Ctx(ctx)->m_stackExpr.pop_back();
                return true;
            default:
                return false;
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinStructExprAc);

        /// <summary>
        ///  BuiltinNumDecAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinNumDecAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        { // 这里面的 Num 只支持 u64 类型 最大值 0xFFFFFFFFFFFFFFFF = 18446744073709551615
            if (food.Length() > 20)
            {
                return false;
            }
            else if (food.Length() == 20)
            { // @TODO 最大不能超过 18446744073709551615

            }

            Ctx(ctx)->m_stackNumDec.push_back(Food::AsInt(food.Data()));
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinNumDecAc);

        /// <summary>
        ///   BuiltinLoopSymbolAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinLoopSymbolAc)
        bool Handler(const Food& food, void* ctx, std::string& err) noexcept override
        { // '?' | '*' | "+" | loop_n | loop_m_n | loop_m_max ;
            switch (food.BranchID())
            {
            case 0: // ?
                Ctx(ctx)->m_stackLoopSymbol.emplace_back(0u, 1u);
                return true;
            case 1: // *
                Ctx(ctx)->m_stackLoopSymbol.emplace_back(0u, SyntaxLoop::Max);
                return true;
            case 2: // +
                Ctx(ctx)->m_stackLoopSymbol.emplace_back(1u, SyntaxLoop::Max);
                return true;
            case 3: // loop_n
                {
                    std::size_t n = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();
                    Ctx(ctx)->m_stackLoopSymbol.emplace_back(n, n);
                }
                return true;
            case 4: // loop_m_n
                {
                    std::size_t n = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();
                    std::size_t m = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();

                    if (m > n)
                    {
                        err = "Illegal Range { " + std::to_string(m) +", " + std::to_string(n) + " }";
                        return false;
                    }

                    Ctx(ctx)->m_stackLoopSymbol.emplace_back(m, n);
                }
                return true;
            case 5: // loop_m_max
                {
                    std::size_t n = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();
                    Ctx(ctx)->m_stackLoopSymbol.emplace_back(n, SyntaxLoop::Max);
                }
                return true;
            default:
                return false;
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinLoopSymbolAc);

        /// <summary>
        ///  BuiltinLoopSymbolOptAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinLoopSymbolOptAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            Ctx(ctx)->m_stackLoopSymbolOpt.push_back(food.LoopCount());
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinLoopSymbolOptAc);

        /// <summary>
        ///  BuiltinLoopExprAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinLoopExprAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::size_t loopSymbolOpt = Ctx(ctx)->m_stackLoopSymbolOpt.back();
            Ctx(ctx)->m_stackLoopSymbolOpt.pop_back();
            assert(loopSymbolOpt <= 1u);

            Variable* structExpr = Ctx(ctx)->m_stackStructExpr.back();
            Ctx(ctx)->m_stackStructExpr.pop_back();

            if (0 == loopSymbolOpt)
            { // 
                Ctx(ctx)->m_stackLoopExpr.push_back(structExpr);
            }
            else
            { // 组建循环 struct :
                const std::pair<std::size_t, std::size_t>& loopCntPair =
                    Ctx(ctx)->m_stackLoopSymbol.back();
                Ctx(ctx)->m_stackLoopSymbol.pop_back();

                SyntaxLoop* loopExpr = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxLoop>(
                    structExpr, loopCntPair.first, loopCntPair.second);
                Ctx(ctx)->m_stackLoopExpr.push_back(loopExpr);
            }

            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinLoopExprAc);

        /// <summary>
        ///  BuiltinSeqExprAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinSeqExprAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::size_t seqEleCnt = food.LoopCount();
            assert(seqEleCnt <= Ctx(ctx)->m_stackLoopExpr.size());
            auto begin = Ctx(ctx)->m_stackLoopExpr.end() - seqEleCnt;
            auto end   = Ctx(ctx)->m_stackLoopExpr.end();

            SyntaxSequence* seqExpr = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxSequence>(_range_arg, begin, end);
            Ctx(ctx)->m_stackSeqExpr.push_back(seqExpr);
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinSeqExprAc);

        /// <summary>
        ///  BuiltinBranchExprSomeAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinBranchExprSomeAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            Ctx(ctx)->m_stackBranchExprSome.push_back(food.LoopCount());
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinBranchExprSomeAc);

        /// <summary>
        ///  BuiltinBranchPairsAc = BuiltinExprAc  
        ///    expr : branch_paris;
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinExprAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::size_t cnt = Ctx(ctx)->m_stackBranchExprSome.back() + 1u;
            Ctx(ctx)->m_stackBranchExprSome.pop_back();

            assert(cnt <= Ctx(ctx)->m_stackSeqExpr.size());
            auto begin = Ctx(ctx)->m_stackSeqExpr.end() - cnt;
            auto end   = Ctx(ctx)->m_stackSeqExpr.end();

            SyntaxBranch* branchPairs = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxBranch>(_range_arg, begin, end);
            Ctx(ctx)->m_stackExpr.push_back(branchPairs);
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinExprAc);

        /// <summary>
        ///  BuiltinProductionStatementAc
        /// </summary>
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinProductionStatementAc)
        bool Handler(const Food& food, void* ctx, std::string& err) noexcept override
        {
            Variable* body = Ctx(ctx)->m_stackExpr.back();
            Ctx(ctx)->m_stackExpr.pop_back();

            auto&& _head = Ctx(ctx)->m_stackHead.back();
            StrRef      headRef = _head.rawRef;
            std::string headData = std::move(_head.strData);
            Ctx(ctx)->m_stackHead.pop_back();

            auto found = Ctx(ctx)->m_varMap.find(headData);
            if (Ctx(ctx)->m_varMap.end() == found)
            {
                if (0 != body->_Name().IsNull() && body->GetAction())
                {// 比如：
                    // ident : $Ident(); # ident 挂了一个 Action
                    // var : ident;      # var 也挂了一个 Action, 那么此时 var 应该是一个 mut_var
                    auto acFound = Ctx(ctx)->m_varActionMap.find(headData);
                    if (Ctx(ctx)->m_varActionMap.end() != acFound)
                    { // head 也挂了一个 Action
                        body = Ctx(ctx)->m_varAlloc.ForceAlloc<MutableVariable>(body); // new body
                        assert(body->IsMutable());
                    }
                }
                Ctx(ctx)->m_varMap.emplace(headData, body); // 直接绑定关系 (直接引用)
                body->_SetName(headRef); // head 原始文本
                return true;
            }
            else
            {
                Variable* var = found->second;
                if (var->IsMutable())
                {
#pragma warning("                  使得 var 的层级最小，（遇到 Action 终止优化）")
                    //if (var 的 mut 只有一层)
                    {
                        var->_SwapMut(body);
                        return true;
                    }
                }
                else
                {
                    err = "BUG xcxxxcxvsefwefwefwefwefwefwefwefwfwefwefwef\n";
                    return false;
                }
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinProductionStatementAc);

#undef _BULITIN_ACTIONS_DEFINE_BEGIN
#undef _BULITIN_ACTIONS_DEFINE_END
} // namespace internal

    // class Lexer
    class Lexer final
    {
    public:
        typedef std::unordered_map<std::string, Variable*> VarsTable;

    public:
        Lexer()
        {
            MakeInternal();
        }

        ~Lexer() noexcept
        {
            DestoryInternalActions();
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
        bool ScanScript(const StrRef& script, const VarsTable&, 
            std::size_t offset, std::string& err) const noexcept
        {
            ActionQueue aq;
            // aq.reserve(2 * ); // @TODO <----
            if (m_internalRoot->Scan(script, offset, aq, err))
            {
                return script.len == offset;
            }

            return false;
        }

        template <class MyAction, class...Args>
        Action* AllocAction(Args&&... args)
        {
            static_assert(std::is_base_of<Action, MyAction>::value, "Error");

            Action* action = new MyAction(std::forward<Args>(args)...);
            m_internalActions.push_back(action);
            return action;
        }

    private:
        template <class T, class... Args,
            class = typename std::enable_if<!std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* AllocDollarFn(StrRef name, Args&&...args) noexcept
        {
            Variable* ret = m_variableAllocator.ForceAlloc<T>(std::forward<Args>(args)...);
            if (nullptr != ret)
            {
                ret->_SetName(name);
            }
            return ret;
        }

        template <class T,
            class = typename std::enable_if<std::is_base_of<internal::SyntaxToken, T>::value>::type>
            Variable* AllocDollarFn(StrRef name, StrRef token,
                Variable* whites = nullptr, Variable* rightAssert = nullptr) noexcept
        {
            return Alloc<T>(name, token, whites, rightAssert);
        }

        template <class T, class... Args,
           class = typename std::enable_if<!std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* Alloc(StrRef name, Args&&...args) noexcept
        {
            Variable* ret = m_variableAllocator.Alloc<T>(std::forward<Args>(args)...);
            if (nullptr != ret)
            {
                if (ret->_Name().IsNull())
                {
                    ret->_SetName(name);
                }
                else
                {
                    assert(0);  // <--------------- debug 
                    return nullptr;
                }
            }
            return ret;
        }

        template <class T, 
            class = typename std::enable_if<std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* Alloc(StrRef name, StrRef token, 
            Variable* whites = nullptr, Variable* rightAssert = nullptr) noexcept
        {
            internal::SyntaxToken* ret 
                = (internal::SyntaxToken*)m_variableAllocator.ForceAlloc<T>(token);
            if (nullptr != ret)
            {
                ret->_SetName(name);
                if (! ret->SetLeftWhites(whites))
                {
                    return nullptr;
                }

                ret->SetRightAssert(rightAssert);
            }
            return ret;
        }

        //// internal: 内置语法
        bool MakeInternal() noexcept
        {
            using namespace internal;

            Variable* white   = AllocDollarFn<BuiltinWhite>("white");
            Variable* comment = AllocDollarFn<BuildinComment>("comment");
            Variable* whites  = Alloc<SyntaxLoop>("whites",
                Alloc<SyntaxBranch>("white_or_comment", white, comment), 0, SyntaxLoop::Max);
            Variable* tokenRightAssert = AllocDollarFn<BuiltinTokenRightZeroWidthAssertion>("tokenRightAssert");

            // ident      : ; # func  (暂时使用函数)
            Variable* ident = AllocDollarFn<BuiltinIdent>("ident", "", whites); // ident 不需要右边界断言
            // head  :  ident; # 正常情况下，先挂 Action, 所以这里知道 head 是 mut 还是直接引用
            Variable* head = Alloc<MutableVariable>("head", ident); // 未定义的变量名 
            // var   :  ident; # 同上
            Variable* var  = Alloc<MutableVariable>("var", ident);  // 已经定义的变量名

            // terminator    : '.*?'  # 或者正则表达式 
            Variable* terminator = AllocDollarFn<BuiltinTerminator>("terminator", "", whites);

            // operand : terminator | func_expr | var;
            Variable* func_expr = Alloc<SyntaxSequence>("func_expr"); // decl
            Variable* operand = Alloc<SyntaxBranch>("operand", terminator, func_expr, var); 

            // expr : 优先级 循环 > 序列 > 分支
            Variable* $0x7B = Alloc<SyntaxToken>("{", "{", whites);
            Variable* $0x7D = Alloc<SyntaxToken>("}", "}", whites);
            Variable* $0x2C = Alloc<SyntaxToken>(",", ",", whites);
            Variable* $0x28 = Alloc<SyntaxToken>("(", "(", whites);
            Variable* $0x29 = Alloc<SyntaxToken>(")", ")", whites);

            // decl expr;
            Variable* expr = Alloc<MutableVariable>("expr"); // mut_var

            // struct_with_bracket : '(' expr ')' ;
            Variable* struct_with_bracket = Alloc<SyntaxSequence>("struct_with_bracket", $0x28, expr, $0x29);

            // struct_expr : operand | struct_with_bracket # 这里 仅能容纳 var ??? 那正则表达式怎么办 ？？？TODO,  可以使用变量！！！！
            Variable* struct_expr = Alloc<SyntaxBranch>("struct_expr", operand, struct_with_bracket);

            // num_dec regex : [1-9][0-9]*|0 ;
            Variable* num_dec = AllocDollarFn<BuiltinNaturalNumDec>("num_dec", "", whites, tokenRightAssert);

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

            // expr      ： branch_pairs;
            assert(expr->IsMutable());
            expr->_SwapMut(branch_pairs);

            // productionStatement :  head expr ';' ; 
            Variable* production_statement = Alloc<SyntaxSequence>("production_statement", head, Alloc<SyntaxToken>(":", ":", whites), expr);

            // semicolon_opt : ','?;
            Variable* semicolon_opt = Alloc<SyntaxLoop>("semicolon_opt", $0x2C, 0u, 1u);

            Variable* let_keyword = Alloc<SyntaxToken>("let_keyword", "let", whites, tokenRightAssert);
            Variable* equal_mark = Alloc<SyntaxToken>("=", "=", whites);
            Variable* where_keyword = Alloc<SyntaxToken>("where", "where", whites, tokenRightAssert);

            // operand_swap : operand '->' operand;
            Variable* operand_swap = Alloc<SyntaxSequence>("operand_swap", operand, Alloc<SyntaxToken>("->", "->", whites), operand);
            // operand_swap_some:  (semicolon_opt operand_swap)*;
            Variable* operand_swap_some = Alloc<SyntaxLoop>("operand_swap_some",
                Alloc<SyntaxSequence>("another_operand_swap", semicolon_opt, operand_swap),
                0, SyntaxLoop::Max);


            //// let var = var2, where a -> b, c -> d; // @TODO 这里是否要求 a 和 c 必须是非终结符或指向终结符的变量？ !!!!!!!!!!!!!! 后者无歧义？？？
            //// let_where : 'let' head '=' var (','? 'where' operand '->' operand (','? operand '->' operand)*)?
            //Variable* where_clause = Alloc<SyntaxSequence>("where_clause", semicolon_opt, where_keyword, operand_swap, operand_swap_some);
            //Variable* where_clause_opt = Alloc<SyntaxLoop>("where_clause_opt", where_clause, 0u, 1u);
            //Variable* let_where = Alloc<SyntaxSequence>("let_where", let_keyword, head, equal_mark, var, where_clause_opt);

            // let_where : 'let' head '=' var ','? 'where' operand '->' operand (','? operand '->' operand)*
            Variable* let_where = Alloc<SyntaxSequence>("let_where", let_keyword, head, equal_mark, var, 
                semicolon_opt, where_keyword, operand_swap, operand_swap_some);

            // let var = $func();
            // func_expr: 'let' var '=' /$func\s*(/ (operand(',' operand)*) ? ')';
            Variable* local_func_prefix = Alloc<SyntaxToken>("local_func_prefix", "$", whites);
            Variable* func_name = Alloc<SyntaxSequence>("func_name", local_func_prefix, ident);
            Variable* func_arg = Alloc<SyntaxBranch>("func_arg", num_dec, operand);
            Variable* suffix_args = Alloc<SyntaxSequence>("suffix_args", $0x2C, func_arg);
            Variable* suffix_args_some = Alloc<SyntaxLoop>("suffix_args_some", suffix_args, 0, SyntaxLoop::Max);
            Variable* func_args = Alloc<SyntaxSequence>("func_args", func_arg, suffix_args_some);
            Variable* func_args_opt = Alloc<SyntaxLoop>("func_args_opt", func_args, 0, 1u);
            //Variable* func_expr = Alloc<SyntaxSequence>("func_expr", func_name, $0x28, func_args_opt, $0x29);
            assert(func_expr->Type().IsSequence());
            ((SyntaxSequence*)func_expr)->AppendVariable(func_name).AppendVariable($0x28).AppendVariable(func_args_opt).AppendVariable($0x29);

            // statement : production_statement | let_where ;
            Variable* statement = Alloc<SyntaxBranch>("statement", production_statement, let_where);

            // statement_with_endmark : statement ';'+;
            Variable* statement_with_endmark = Alloc<SyntaxSequence>("statement_with_endmark", statement,
                Alloc<SyntaxLoop>("statement_endmark", Alloc<SyntaxToken>(";", ";", whites), 1u, SyntaxLoop::Max), whites);

            // root:         statement_with_endmark + ;
            m_internalRoot = Alloc<SyntaxLoop>("root", statement_with_endmark, 1u, SyntaxLoop::Max);

//
//           /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//           // Action
//           Bind(terminator, AllocAction<BuiltinTerminatorAc>()) || Error("BUG: bind 'terminator' action");
//           Bind(ident, AllocAction<BuiltinIdentAc>())           || Error("BUG: bind 'ident' action");
//           Bind(head, AllocAction<BuiltinHeadAc>())             || Error("BUG: bind 'head' action");
//           Bind(var, AllocAction<BuiltinVarAc>())               || Error("BUG: bind 'var' action");
//           Bind(operand, AllocAction<BuiltinOperandAc>())       || Error("BUG: bind 'operand' action");
//           Bind(struct_expr, AllocAction<BuiltinStructExprAc>())|| Error("BUG: bind 'struct_expr' action");
//           Bind(num_dec, AllocAction<BuiltinNumDecAc>())        || Error("BUG: bind 'num_dec' action");
//           Bind(loop_symbol, AllocAction<BuiltinLoopSymbolAc>())|| Error("BUG: bind 'loop_symbol' action");
//           Bind(loop_symbol_opt, AllocAction<BuiltinLoopSymbolOptAc>()) || Error("BUG: bind 'loop_symbol_opt' action");
//           Bind(loop_expr, AllocAction<BuiltinLoopExprAc>())    || Error("BUG: bind 'loop_expr' action");
//           Bind(seq_expr, AllocAction<BuiltinSeqExprAc>())      || Error("BUG: bind 'seq_expr' action");
//           Bind(branch_expr_some, AllocAction<BuiltinBranchExprSomeAc>()) || Error("BUG: bind 'branch_expr_some' action");
//           Bind(expr, AllocAction<BuiltinExprAc>())             || Error("BUG: bind 'expr' action");
//           Bind(production_statement, AllocAction<BuiltinProductionStatementAc>()) || Error("BUG: bind 'production_statement' action");
//

            return true;
        } // MakeInteranlSyntax

        bool Bind(Variable* var, Action* action) noexcept
        {
            if (nullptr == var || nullptr == action)
            {
                return false;
            }

            if (nullptr == var->GetAction())
            {
                var->SetAction(action);
                return true;
            } 
            else
            { // 再加一层 Action ???
#pragma warning("")
                // 兼容 
                // ident : $Ident();
                // head  : ident
                // var   : ident;

                // 其中：
                // ident <- action
                // head  <- action
                // var   <- action
                return false;
            }
        }

        bool Bind(const std::string& var, Action* action) noexcept
        {
            Variable* v = nullptr;
            // TODO
            // -----<
            return Bind(v, action);
        }

        static bool Error(StrRef err)
        {
            throw(err.ToStdString());
        }

        void DestoryInternalActions() noexcept
        {
            for (Action* action : m_internalActions)
            {
                delete action;
            }
            m_internalActions.clear();
        }

    private:
        Variable*                                  m_internalRoot;
        VariableAllocator                          m_variableAllocator;  // 该 Allocator 可能是过度设计，后面要简化这个结构和逻辑！！！！！@TODO
        std::deque<Action*>                        m_internalActions;    // 内置文法动作集合            
        std::unordered_map<std::string, Variable*> m_userVariablesMap;   // 输出(1) 
        //ActionAllocator                            m_actionAllocator;
    }; // class Lexer

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