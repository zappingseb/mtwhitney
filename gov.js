if (typeof QSI === 'undefined') {
    QSI = {};
}
QSI.config = {
    "hostedJSLocation": "https://siteintercept.qualtrics.com/dxjsmodule/",
    "zoneId": "ZN_0CfkVh4TbZPloSp"
};
QSI.overrides = {
    "baseURL": "https://siteintercept.qualtrics.com"
};
QSI.shouldStripQueryParamsInQLoc = false;
try {
    if (void 0 === window.QSI && (window.QSI = {}), void 0 === QSI.reg && (QSI.reg = {}), void 0 === QSI.ed && (QSI.ed = {}), void 0 === QSI.reqID && (QSI.reqID = {}), QSI.overrides = QSI.overrides || {}, void 0 === QSI.isFullDbgInitialized && (QSI.isFullDbgInitialized = !1), QSI.initFullDbg = function() {
            !0 !== QSI.isFullDbgInitialized && (QSI.isFullDbgInitialized = !0, QSI.dbg = {
                log: function() {},
                c: function(e) {
                    try {
                        console.log(e)
                    } catch (e) {}
                    QSI.dbg.log(e)
                },
                d: function(e) {
                    try {
                        console.dir(e)
                    } catch (e) {}
                    QSI.dbg.log(e)
                },
                t: function(e) {
                    try {
                        console.trace()
                    } catch (e) {}
                    QSI.dbg.log(e)
                },
                e: function(t) {
                    var i = "error";
                    t.message && (i = t.message);
                    try {
                        console.log(i), console.error(t)
                    } catch (e) {
                        try {
                            console.log(i), console.log(t)
                        } catch (e) {}
                    }
                    QSI.dbg.log(t)
                }
            })
        }, -1 === window.location.href.indexOf("Q_DEBUG") ? QSI.dbg = {
            log: function() {},
            c: function(e) {
                QSI.dbg.log(e)
            },
            d: function(e) {
                QSI.dbg.log(e)
            },
            t: function(e) {
                QSI.dbg.log(e)
            },
            e: function(e) {
                QSI.dbg.log(e)
            }
        } : QSI.initFullDbg(), void 0 === QSI.config && (QSI.config = {}), void 0 === QSI.global && (QSI.global = {
            currentZIndex: 2e9,
            intercepts: {},
            eventTrackers: []
        }), QSI.global.hostedJSLocation = QSI.config.hostedJSLocation || QSI.config.clientBaseURL, QSI.global.legacyId = QSI.config.interceptId || QSI.config.zoneId || QSI.config.targetingId || QSI.global.ID, !QSI.global.legacyId) throw "You must specify a zoneId or zoneId and interceptId";
    QSI.getBaseURLFromConfigAndOverrides = function(e, t) {
        t = t || {}, (e = e || {}).zoneId = e.zoneId || "", e.brandId = e.brandId || "";
        var i = "",
            n = !1;
        if (t.baseURL ? (n = !0, 0 === (i = t.baseURL).indexOf("https://") ? i = i.substring(8) : 0 === i.indexOf("http://") ? i = i.substring(7) : 0 === i.indexOf("//") && (i = i.substring(2))) : i = "siteintercept.qualtrics.com", !n && e.brandId) {
            if (!e.zoneId) throw "You must specify a zoneId";
            i = e.zoneId.replace("_", "").toLowerCase() + "-" + e.brandId.toLowerCase() + "." + i
        }
        return i
    }, QSI.global.baseURL = QSI.getBaseURLFromConfigAndOverrides(QSI.config, QSI.overrides), QSI.global.baseURL = "//" + QSI.global.baseURL, QSI.global.clientVersion = "1.6.0", QSI.global.clientType = "web", void 0 === QSI.global.isHostedJS && (QSI.global.isHostedJS = function() {
        return "hostedjs" === QSI.global.clientType
    }), QSI.global.isHostedJS() ? QSI.global.enableJSSanitization = void 0 === QSI.config.enableJSSanitization || QSI.config.enableJSSanitization : QSI.global.enableJSSanitization = !1, QSI.baseURL = QSI.baseURL || QSI.overrides.siBaseURL || "https:" + QSI.global.baseURL + "/WRSiteInterceptEngine/", QSI.Browser = {}, QSI.profile || (QSI.profile = {
        namespace: "QSI_",
        set: function(e, t, i, n) {
            if (void 0 === e || void 0 === t || void 0 === i) throw new Error("To few arguments");
            try {
                var r = this.getStorage(n),
                    a = this.namespace + e,
                    o = r.getItem(a);
                (o = o ? JSON.parse(o) : {})[t] = i, o = JSON.stringify(o), r.setItem(a, o)
            } catch (e) {
                QSI.dbg.e("error setting profile item"), QSI.dbg.e(e)
            }
        },
        get: function(e, t, i) {
            var n = this.getStorage(i),
                r = this.namespace + e,
                a = n.getItem(r);
            return a ? (a = JSON.parse(a), t ? a[t] ? a[t] : null : a) : null
        },
        erase: function(e, t, i) {
            var n = this.getStorage(i),
                r = this.namespace + e;
            if (t) {
                var a = JSON.parse(n.getItem(r));
                delete a[t], a = JSON.stringify(a), n.setItem(r, a)
            } else n.removeItem(r)
        },
        getStorage: function(e) {
            if (this.hasSessionStorage()) return e ? localStorage : sessionStorage;
            if (QSI.UserDataStorage) {
                var t = QSI.UserDataStorage;
                return e ? t.isPermanent(!0) : t.isPermanent(!1), t
            }
            return QSI.CookieStorage
        },
        hasSessionStorage: function() {
            var e = "qualtricssessionstoragetestkey",
                t = window.sessionStorage;
            try {
                return t.setItem(e, e), t.removeItem(e), !0
            } catch (e) {
                return !1
            }
        }
    }), void 0 === QSI.util && (QSI.util = {
        $: function(e) {
            return "string" == typeof e && (e = document.getElementById(e)), e
        },
        forOwn: function(e, t) {
            if (e && e instanceof Object && this.isFunction(t))
                for (var i in e) Object.prototype.hasOwnProperty.call(e, i) && t(e[i], i, e)
        },
        build: function(e, i, t) {
            var n = document.createElement(e);
            if (i) {
                var r = this;
                QSI.util.forOwn(i, function(e, t) {
                    switch (t) {
                        case "style":
                            r.setStyle(n, i[t]);
                            break;
                        case "className":
                            n.className = i[t];
                            break;
                        case "id":
                            n.id = i[t];
                            break;
                        default:
                            n.setAttribute(t, i[t])
                    }
                })
            }
            if (t)
                if (QSI.util.isString(t)) "style" === e && n.styleSheet ? n.styleSheet.cssText = t : n.appendChild(document.createTextNode(String(t)));
                else if (QSI.util.isArray(t))
                for (var a = 0, o = t.length; a < o; a++) {
                    var s = t[a];
                    "string" == typeof s || "number" == typeof s ? n.appendChild(document.createTextNode(String(s))) : s && s.nodeType && n.appendChild(s)
                }
            return n
        },
        setStyle: function(i, n) {
            QSI.util.forOwn(n, function(e, t) {
                try {
                    i.style[t] = n[t]
                } catch (e) {
                    QSI.dbg.e(e)
                }
            })
        },
        isString: function(e) {
            return "string" == typeof e
        },
        isArray: function(e) {
            return "object" == typeof e && e instanceof Array
        },
        getQueryParam: function(e, t) {
            t = t.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
            var i = new RegExp("[\\?&]" + t + "=([^&#]*)").exec(e);
            return null === i ? "" : i[1]
        },
        observe: function(e, t, i, n) {
            this.obs = this.obs || [], e && (this.obs.push({
                el: e,
                e: t,
                f: i,
                preventRemove: n || !1
            }), e.addEventListener ? e.addEventListener(t, i, !1) : e.attachEvent ? e.attachEvent("on" + t, i) : e["on" + this.capFirst(t)] && (e["on" + this.capFirst(t)] = i))
        },
        stopObserving: function(e, t, i) {
            e.removeEventListener ? e.removeEventListener(t, i, !1) : e.detachEvent ? e.detachEvent("on" + t, i) : e["on" + this.capFirst(t)] && (e["on" + this.capFirst(t)] = null)
        },
        removeObservers: function() {
            var t = this;
            this.each(this.obs || [], function(e) {
                e.preventRemove || t.stopObserving(e.el, e.e, e.f)
            })
        },
        remove: function(e) {
            e && e.parentNode && e.parentNode.removeChild(e)
        },
        isFunction: function(e) {
            return "function" == typeof e || !1
        },
        capFirst: function(e) {
            return e.charAt(0).toUpperCase() + e.slice(1)
        },
        each: function(e, t) {
            var i = e.length;
            if (i)
                for (var n = 0; n < i; n++) t(e[n], n)
        }
    }), QSI.API && !window.QTest || (QSI.API = {
        load: function() {
            function e(n) {
                try {
                    if (QSI.reg || this.unloading) return void n.reject();
                    void 0 === window.QSI && (window.QSI = {}), void 0 === QSI.reg && (QSI.reg = {}), void 0 === QSI.ed && (QSI.ed = {}), void 0 === QSI.reqID && (QSI.reqID = {}), void 0 === QSI.Request && (QSI.Request = {}), void 0 === QSI.styleElements && (QSI.styleElements = []), QSI.util.forOwn(QSI.reqID, function(e, t) {
                        var i = {
                            loadingFromAPI: !0
                        };
                        i.id = t, "Editing" === QSI.version && (i.version = "0"), void 0 !== QSI.global.clientVersion && null !== QSI.global.clientVersion && (i.Q_CLIENTVERSION = QSI.global.clientVersion), void 0 !== QSI.global.clientType && null !== QSI.global.clientType && (i.Q_CLIENTTYPE = QSI.global.clientType), 0 === t.search(/ZN/) ? i.ZoneID = t : i.InterceptID = t, QSI.isDebug && (i.Q_DEBUG = null), i.deferred = n, QSI.Orchestrator.load(i)
                    })
                } catch (e) {
                    QSI.dbg.e(e)
                }
            }
            try {
                var t = QSI.Orchestrator.Deferred(),
                    i = t.promise();
                return QSI.PendingQueue || (QSI.PendingQueue = []), QSI.LoadingState && 0 < QSI.LoadingState.length ? QSI.PendingQueue.push(e.bind(this, t)) : e.bind(this, t)(), i
            } catch (e) {
                QSI.dbg.e(e)
            }
        },
        unload: function() {
            try {
                if (QSI.PendingQueue || (QSI.PendingQueue = []), QSI.LoadingState && 0 < QSI.LoadingState.length) return void QSI.PendingQueue.push(QSI.API.unload);
                if (this.unloading = !0, QSI.reg && (QSI.util.forOwn(QSI.reg, function(e, t) {
                        QSI.reg[t].remove()
                    }), QSI.util.removeObservers()), QSI.debug && (QSI.util.remove(QSI.util.$("QSI_Debug")), QSI.debuggerHasDisplayed = !1), QSI.styleElements)
                    for (var e = QSI.styleElements, t = 0; t < e.length; t++) QSI.util.remove(e[t]);
                QSI.reg = void 0, QSI.Request = void 0, QSI.styleElements = void 0, this.unloading = !1
            } catch (e) {
                QSI.dbg.e(e)
            }
        },
        run: function() {
            try {
                if (QSI.PendingQueue || (QSI.PendingQueue = []), QSI.LoadingState && 0 < QSI.LoadingState.length) return void QSI.PendingQueue.push(QSI.API.run);
                QSI.InterceptsRan || void 0 === QSI.reg || QSI.RunIntercepts(null, !0)
            } catch (e) {
                QSI.dbg.e(e)
            }
        },
        Events: {
            increment: function(e) {
                try {
                    QSI.EventTracker.track(e)
                } catch (e) {
                    QSI.dbg.e(e)
                }
            },
            count: function(e) {
                try {
                    return QSI.EventTracker.get(e)
                } catch (e) {
                    QSI.dbg.e(e)
                }
            },
            push: function(e) {
                try {
                    QSI.EventTracker.track(e)
                } catch (e) {
                    QSI.dbg.e(e)
                }
            }
        }
    }), QSI.ContactFrequency = {
        contactFrequencyRequestTimeout: 1e4,
        contactFrequencyCheckResults: {},
        contactFrequencyCheckStates: {
            CAN_CONTACT: "CAN_CONTACT",
            DO_NOT_CONTACT: "DO_NOT_CONTACT",
            NOT_CHECKED: "NOT_CHECKED"
        },
        checkContactFrequencyRules: function(e, a) {
            var t = QSI.Orchestrator.Deferred(),
                o = {};
            return QSI.util.forOwn(e, function(e, t) {
                this.contactFrequencyCheckResults[t] = {
                    status: this.contactFrequencyCheckStates.NOT_CHECKED,
                    brandId: null,
                    surveyId: null
                };
                var i = e.Intercept;
                if (!QSI.util.shouldPreventRepeatedDisplay(t, i.DisplayOptions) && this.shouldCheckContactFrequency(i)) {
                    var n = this.getIQDirectorySettings(i);
                    if (n) {
                        var r = this.getTargetSurveyId(i, e.Targeting);
                        r && (n.surveyId = r, this.contactFrequencyCheckResults[t].surveyId = r), this.contactFrequencyCheckResults[t].brandId = a, n.brandId = a, o[t] = n, this.contactFrequencyCheckResults[t].status = this.contactFrequencyCheckStates.DO_NOT_CONTACT
                    } else this.contactFrequencyCheckResults[t].status = this.contactFrequencyCheckStates.DO_NOT_CONTACT
                }
            }.bind(this)), 0 !== Object.keys(o).length ? this.sendContactFrequencyRequest(o, t) : t.resolve(), t.promise()
        },
        shouldCheckContactFrequency: function(e) {
            return e.DisplayOptions && e.DisplayOptions.useContactFrequencyRules
        },
        getIQDirectorySettings: function(e) {
            if (!e.DisplayOptions) return null;
            var t = e.DisplayOptions,
                i = this.resolveContactFrequencyExternalReference(t.contactFrequencyRefLocator, t.contactFrequencyRefExpression);
            if (!i) return QSI.dbg.e("Could not retrieve external reference ID for Contact Frequency Check"), null;
            if (!("string" == typeof i || i instanceof String)) return QSI.dbg.e("External reference ID must be a string for Contact Frequency Check"), null;
            var n = t.contactFrequencyDirectoryID;
            if (!n) return QSI.dbg.e("Missing directoryId for Contact Frequency Check"), null;
            var r = t.contactFrequencyUserID;
            return r ? {
                extRef: i,
                directoryId: n,
                userId: r
            } : (QSI.dbg.e("Missing User Id for Contact Frequency Check"), null)
        },
        getTargetSurveyId: function(e, t) {
            if (!t.Decision || !t.Decision.ActionSetID) return null;
            var i = t.Decision.ActionSetID;
            return e.ActionSets && e.ActionSets[i] && e.ActionSets[i].Target && "Survey" === e.ActionSets[i].Target.Type ? e.ActionSets[i].Target.PrimaryElement : null
        },
        resolveContactFrequencyExternalReference: function(e, t) {
            if (!t) return QSI.dbg.e("No reference expression used for contact frequency check"), null;
            switch (e) {
                case "JavaScript":
                    return QSI.EmbeddedData.getJavaScriptValue(t.expression);
                case "Cookie":
                    return QSI.cookie.get(t.expression);
                default:
                    return QSI.dbg.e("Reference locator used for contact frequency check is unsupported:" + e), null
            }
        },
        sendContactFrequencyRequest: function(e, i) {
            var n = "https://" + QSI.global.brandDC + "/dx-iqd-proxy/check-frequency-rules";
            return void QSI.util.sendHttpRequest({
                type: "POST",
                url: n,
                header: {
                    "Content-type": "application/json"
                },
                data: JSON.stringify(e),
                successCallback: function(e) {
                    try {
                        var t;
                        if (e.response && (t = JSON.parse(e.response)), "object" != typeof t) throw new Error("ContactFrequency check came back with invalid response: " + results);
                        QSI.util.forOwn(t, function(e, t) {
                            null !== e ? !0 === e.passesFrequencyRules && (this.contactFrequencyCheckResults[t].status = this.contactFrequencyCheckStates.CAN_CONTACT, this.contactFrequencyCheckResults[t].contactId = e.contactId) : QSI.dbg.e("ContactFrequency check for intercept:" + t + " errored; defaulting to no show")
                        }.bind(this)), i.resolve()
                    } catch (e) {
                        QSI.dbg.e(e), i.resolve()
                    }
                }.bind(this),
                errorCallback: function(e) {
                    var t = "Call to ContactFrequency endpoint: " + n + " failed.";
                    e.status && (t = t + " Status:" + e.status);
                    e.response && (t = t + " Response:" + e.response);
                    QSI.dbg.e(t), i.resolve()
                }.bind(this),
                timeout: this.contactFrequencyRequestTimeout,
                timeoutCallback: function() {
                    QSI.dbg.e("ContactFrequency check POST request to:" + n + " timed out"), i.resolve()
                }
            })
        },
        recordContactFrequencyRequest: function(e) {
            var i = "https://" + QSI.global.brandDC + "/dx-iqd-proxy/record-contact";
            return void QSI.util.sendHttpRequest({
                type: "POST",
                url: i,
                header: {
                    "Content-type": "application/json"
                },
                data: JSON.stringify(e),
                errorCallback: function(e) {
                    var t = "Call to ContactFrequency endpoint: " + i + " failed.";
                    e.status && (t = t + " Status:" + e.status);
                    e.response && (t = t + " Response:" + e.response);
                    QSI.dbg.e(t)
                }.bind(this),
                timeout: this.contactFrequencyRequestTimeout,
                timeoutCallback: function() {
                    QSI.dbg.e("ContactFrequency record POST request to:" + i + " timed out")
                }
            })
        }
    }, QSI.AssetManager = {
        loadedScripts: {},
        promiseFetch: function(e, t, i) {
            var n = QSI.Orchestrator.Deferred(),
                r = new XMLHttpRequest;
            return r.open(e, t, !0), r.withCredentials = !0, "POST" === e && r.setRequestHeader("Content-type", "application/x-www-form-urlencoded"), r.onreadystatechange = function() {
                4 === r.readyState && (200 === r.status ? n.resolve(r.responseText) : n.reject(r.responseText))
            }, r.send(i), n.promise()
        },
        generateDefinitionRequestURL: function(e, t, i, n) {
            var r = QSI.baseURL + "Asset.php?",
                a = [];
            return t = t || i.version, a.push("Module=" + e), a.push("Version=" + t), null != n && a.push("Q_InterceptID=" + n), null === i.Q_NOCACHE && a.push("Q_NOCACHE"), QSI.CORSOrigin && a.push("Q_ORIGIN=" + QSI.CORSOrigin), void 0 !== i.Q_CLIENTVERSION && null !== i.Q_CLIENTVERSION && a.push("Q_CLIENTVERSION=" + i.Q_CLIENTVERSION), void 0 !== i.Q_CLIENTTYPE && null !== i.Q_CLIENTTYPE && a.push("Q_CLIENTTYPE=" + i.Q_CLIENTTYPE), r += a.join("&")
        },
        loadDefinition: function(e, t) {
            var i = QSI.Orchestrator.Deferred(),
                n = new XMLHttpRequest;
            return n.open("GET", e, !0), n.onreadystatechange = function() {
                if (4 === n.readyState)
                    if (200 === n.status) try {
                        var e = JSON.parse(n.responseText);
                        e.Error ? i.reject(e) : (t(e), i.resolve(n.responseText))
                    } catch (e) {
                        i.reject(n.responseText)
                    } else i.reject(n.responseText)
            }, n.send(), i.promise()
        },
        promiseLoadIntercept: function(t, e, i) {
            var n = QSI.AssetManager.generateDefinitionRequestURL(e.InterceptID, e.InterceptRevision, i);
            return QSI.AssetManager.loadDefinition(n, function(e) {
                QSI.Request[t].Intercepts[e.InterceptDefinition.InterceptID].Intercept = e.InterceptDefinition
            })
        },
        promiseLoadCreative: function(t, i, e) {
            if ("CR_NoCreative" === i.Decision.Creative.ID) return QSI.Request[t].Intercepts[i.InterceptID].Creative = null, QSI.Orchestrator.Deferred().resolve(null);
            var n = QSI.AssetManager.generateDefinitionRequestURL(i.Decision.Creative.ID, i.Decision.Creative.Revision, e, i.InterceptID);
            return QSI.AssetManager.loadDefinition(n, function(e) {
                QSI.util.isLegacyCreative(e.CreativeDefinition.Type) && (e.CreativeDefinition = QSI.AssetManager.sortCreativeDefinition(e.CreativeDefinition)), QSI.Request[t].Intercepts[i.InterceptID].Creative = e.CreativeDefinition
            })
        },
        promiseLoadPopUnderTarget: function(t, i, e) {
            var n = QSI.AssetManager.generateDefinitionRequestURL(i.Decision.PopUnderTarget.ID, i.Decision.PopUnderTarget.Revision, e, i.InterceptID);
            return QSI.AssetManager.loadDefinition(n, function(e) {
                QSI.Request[t].Intercepts[i.InterceptID].PopUnderTarget = e.CreativeDefinition
            })
        },
        promiseLoadScript: function(e, t) {
            var i;
            if (this.alreadyFetched(e)) return (i = QSI.Orchestrator.Deferred()).resolve(), i.promise();
            i = QSI.Orchestrator.Deferred();
            var n = document.createElement("script");
            n.src = QSI.global.hostedJSLocation + e + "Module.js?";
            var r = [];
            return void 0 !== QSI.Orchestrator && void 0 !== QSI.Orchestrator.getClientVersionQueryString && r.push(QSI.Orchestrator.getClientVersionQueryString()), -1 === window.location.href.indexOf("Q_DEBUG") && !QSI.config.debug || r.push("Q_DEBUG=true"), n.src += r.join("&"), n.defer = !0, n.addEventListener("load", function() {
                try {
                    !0 === QSI.wrongModuleVersionRequested && (i.reject(), QSI.dbg.e("Script: " + e + " failed to load because an unavailable version (" + t + ") was requested.")), QSI.AssetManager.loadedScripts[e] = n, i.resolve()
                } catch (e) {
                    "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                }
            }, !1), n.addEventListener("error", function() {
                try {
                    i.reject(), QSI.dbg.e("Script: " + e + " failed to load.")
                } catch (e) {
                    "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                }
            }), "Core" === e && QSI.global && QSI.global.latency && QSI.global.latency.si && (QSI.global.latency.si.coreModuleLatencyStartTime = Date.now()), document.body.appendChild(n), i.promise()
        },
        alreadyFetched: function(e) {
            return e in QSI.AssetManager.loadedScripts
        },
        sortCreativeDefinition: function(e) {
            if (e && e.Options && e.Options.elements && e.Options.elements.Elements) {
                var t = e.Options.elements.Elements;
                t = QSI.util.stableSort(t, function(e, t) {
                    return Number(e.style.zIndex) < Number(t.style.zIndex) ? -1 : Number(e.style.zIndex) > Number(t.style.zIndex) ? 1 : 0
                });
                for (var i = 0; i < t.length; i++) t[i].style && t[i].style.zIndex && (t[i].style.zIndex = QSI.global.currentZIndex++);
                return t = QSI.util.stableSort(t, function(e, t) {
                    return Number(e.position.left) < Number(t.position.left) ? -1 : Number(e.position.left) > Number(t.position.left) ? 1 : 0
                }), t = QSI.util.stableSort(t, function(e, t) {
                    return Number(e.position.top) < Number(t.position.top) ? -1 : Number(e.position.top) > Number(t.position.top) ? 1 : 0
                }), e.Options.elements.Elements = t, e
            }
            return e
        }
    }, QSI.CreativeManager = {
        isCreativeSupported: function(e) {
            return !!QSI.CreativeManager["run" + e]
        },
        runEmpty: function(e) {
            var t;
            t = new QSI.Empty({
                id: e.interceptID,
                type: QSI.util.creativeTypes.EMPTY
            }), QSI.reg[e.interceptID] = t
        },
        runWebResponsiveDialog: function(e) {
            var t, i, n, r;
            t = e.creative.Type, i = e.creative.Options.Layout, n = QSI.BuildResponsiveElementModule.PARENT_CONTAINER_CLASS, r = new QSI.WebResponsive[t][i]({
                id: e.interceptID,
                type: t,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                hasCreativeEmbeddedTarget: QSI.CreativeManager.Utilities.hasCreativeEmbeddedTarget(e.creative),
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                elements: e.creative.Options,
                displayOptions: e.creative.Options.displayOptions,
                resetStyle: QSI.CreativeManager.Utilities.getWebResponsiveResetStyle(n)
            }), QSI.reg[e.interceptID] = r
        },
        runFeedbackButton: function(e) {
            var t, i;
            t = e.creative.Type, i = new QSI.FeedbackButton({
                id: e.interceptID,
                type: t,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                elements: e.creative.Options,
                resetStyle: QSI.CreativeManager.Utilities.getWebResponsiveResetStyle("QSIFeedbackButton")
            }), QSI.reg[e.interceptID] = i
        },
        runFeedbackLink: function(e) {
            var t;
            t = new QSI.FeedbackLink({
                id: e.interceptID,
                type: QSI.util.creativeTypes.FEEDBACK_LINK,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                insertionLocation: e.actionSet.ActionOptions.displayElement ? e.actionSet.ActionOptions.displayElement : e.id,
                displayOptions: e.creative.Options,
                resetStyle: QSI.CreativeManager.Utilities.getResetStyle("QSIFeedbackLink")
            }), QSI.reg[e.interceptID] = t
        },
        runHTTPRedirect: function(e) {
            var t;
            QSI.global.isHostedJS() ? QSI.dbg.c("Creative type '" + QSI.util.creativeTypes.HTTP_REDIRECT + "' is not supported.") : (t = new QSI.HTTPRedirect({
                id: e.interceptID,
                type: QSI.util.creativeTypes.HTTP_REDIRECT,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions
            }), QSI.reg[e.interceptID] = t)
        },
        runIFrame: function(e) {
            var t;
            t = new QSI.IFrame({
                id: e.interceptID,
                type: QSI.util.creativeTypes.IFRAME,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                iframeOptions: e.creative.Options,
                insertionLocation: e.actionSet.ActionOptions.displayElement ? e.actionSet.ActionOptions.displayElement : e.id
            }), QSI.reg[e.interceptID] = t
        },
        runInfoBar: function(n) {
            ! function() {
                var e = {
                    id: n.interceptID,
                    type: QSI.util.creativeTypes.INFO_BAR,
                    targetURL: n.decision.Target.URL,
                    targetURLOrigin: n.decision.Target.OriginalURLOrigin,
                    impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                        interceptID: n.interceptID,
                        creativeID: n.decision.Creative.ID,
                        asid: n.decision.ActionSetID
                    }),
                    interceptDisplayOptions: n.intercept.DisplayOptions,
                    actionOptions: n.actionSet.ActionOptions,
                    displayOptions: n.creative.Options.displayOptions
                };
                if (n.creative.Options.elements) e.elements = n.creative.Options.elements, e.resetStyle = QSI.CreativeManager.Utilities.getResetStyle("QSIInfoBar");
                else {
                    e.infoBarOptions = n.creative.Options;
                    var t = QSI.CreativeManager.Utilities.parsePipedText(n.creative.Options.content);
                    t && 0 < t.length && (e.locators = t)
                }
                var i = new QSI.InfoBar(e);
                QSI.reg[n.interceptID] = i
            }()
        },
        runLink: function(e) {
            var t, i;
            t = QSI.CreativeManager.Utilities.parsePipedText(e.creative.Options.linkText), i = new QSI.Link({
                id: e.interceptID,
                type: QSI.util.creativeTypes.LINK,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                text: e.creative.Options.linkText,
                insertionLocation: e.actionSet.ActionOptions.displayElement ? e.actionSet.ActionOptions.displayElement : e.id,
                locators: t && 0 < t.length ? t : null
            }), QSI.reg[e.interceptID] = i
        },
        runNoCreative: function(e) {
            var t;
            t = new QSI.NoCreative({
                id: e.interceptID,
                type: QSI.util.creativeTypes.NO_CREATIVE,
                actionOptions: e.actionSet.ActionOptions,
                interceptDisplayOptions: e.intercept.DisplayOptions
            }), QSI.reg[e.interceptID] = t
        },
        runPopOver: function(e) {
            var t;
            t = new QSI.PopOver({
                id: e.interceptID,
                type: QSI.util.creativeTypes.POP_OVER,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                hasCreativeEmbeddedTarget: QSI.CreativeManager.Utilities.hasCreativeEmbeddedTarget(e.creative),
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                elements: e.creative.Options.elements,
                displayOptions: e.creative.Options.displayOptions,
                resetStyle: QSI.CreativeManager.Utilities.getResetStyle("QSIPopOver")
            }), QSI.reg[e.interceptID] = t
        },
        runPopUnder: function(a) {
            ! function() {
                if (!("Chrome" === QSI.Browser.name && 29 < QSI.Browser.version)) {
                    var e = a.creative.Options,
                        t = {
                            id: a.interceptID,
                            type: QSI.util.creativeTypes.POP_UNDER,
                            targetURL: a.decision.Target.URL,
                            targetURLOrigin: a.decision.Target.OriginalURLOrigin,
                            impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                                interceptID: a.interceptID,
                                creativeID: a.decision.Creative.ID,
                                asid: a.decision.ActionSetID
                            }),
                            interceptDisplayOptions: a.intercept.DisplayOptions,
                            actionOptions: a.actionSet.ActionOptions
                        };
                    if (a.creative.Options.elements) {
                        t.elements = a.creative.Options.elements;
                        var i = "body { background-color: " + e.baseElement.style.backgroundColor + "; } .QSIPopUnder .close { color: #000000; text-decoration: none; } ";
                        t.resetStyle = i + QSI.CreativeManager.Utilities.getResetStyle("QSIPopUnder"), e.width = e.baseElement.style.width, e.height = e.baseElement.style.height
                    } else t.locators = QSI.CreativeManager.Utilities.parsePipedText(null);
                    e.helperScriptSrc = QSI.baseURL + "Orchestrator.php?InterceptID=" + a.interceptID + "&Q_Type=PopUnderHelper";
                    var n = {
                        onPopForwardShowTarget: e.showTargetOnPopForward,
                        showOnPageChange: e.showOnPageChange,
                        showOnSiteExit: e.showOnSiteExit,
                        checkThreshold: 3,
                        watchInterval: 1e3,
                        targetWidth: a.actionSet.ActionOptions.targetWidth,
                        targetHeight: a.actionSet.ActionOptions.targetHeight,
                        targetFullScreen: a.actionSet.ActionOptions.targetFullScreen,
                        impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                            interceptID: a.interceptID,
                            creativeID: a.decision.Creative.ID,
                            asid: a.decision.ActionSetID
                        })
                    };
                    t.popunderOptions = e, t.popunderHelperOptions = n;
                    var r = new QSI.PopUnder(t);
                    QSI.reg[a.interceptID] = r
                }
            }()
        },
        runPopUnderHelper: function(e) {
            QualtricsSI[e.interceptID].popunderCheckThreshold = null, QualtricsSI[e.interceptID].popunderWatchInterval = null, QualtricsSI.PopunderWatcherModule.startWatching(e.interceptID)
        },
        runPopUp: function(e) {
            var t;
            t = new QSI.PopUp({
                id: e.interceptID,
                type: QSI.util.creativeTypes.POP_UP,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                creativeOptions: e.creative.Options
            }), QSI.reg[e.interceptID] = t
        },
        runRelay: function(e) {
            var t;
            t = new QSI.Relay({
                id: e.interceptID,
                type: QSI.util.creativeTypes.RELAY,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                elements: e.creative.Options.elements,
                containerElement: e.creative.Options.baseElement,
                displayOptions: e.creative.Options.displayOptions,
                resetStyle: QSI.CreativeManager.Utilities.getResetStyle("QSIRelay")
            }), QSI.reg[e.interceptID] = t
        },
        runSlider: function(e) {
            var t;
            t = new QSI.Slider({
                id: e.interceptID,
                type: QSI.util.creativeTypes.SLIDER,
                targetURL: e.decision.Target.URL,
                targetURLOrigin: e.decision.Target.OriginalURLOrigin,
                hasCreativeEmbeddedTarget: QSI.CreativeManager.Utilities.hasCreativeEmbeddedTarget(e.creative),
                impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                    interceptID: e.interceptID,
                    creativeID: e.decision.Creative.ID,
                    asid: e.decision.ActionSetID
                }),
                interceptDisplayOptions: e.intercept.DisplayOptions,
                actionOptions: e.actionSet.ActionOptions,
                elements: e.creative.Options.elements,
                displayOptions: e.creative.Options.displayOptions,
                resetStyle: QSI.CreativeManager.Utilities.getResetStyle("QSISlider")
            }), QSI.reg[e.interceptID] = t
        },
        runSocialMedia: function(t) {
            ! function() {
                if (QSI.global.isHostedJS()) QSI.dbg.c("Creative type '" + QSI.util.creativeTypes.SOCIAL_MEDIA + "' is not supported.");
                else {
                    var e = new QSI.SocialMedia({
                        id: t.interceptID,
                        type: QSI.util.creativeTypes.SOCIAL_MEDIA,
                        impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                            interceptID: t.interceptID,
                            creativeID: t.decision.Creative.ID,
                            asid: t.decision.ActionSetID
                        }),
                        interceptDisplayOptions: t.intercept.DisplayOptions,
                        actionOptions: t.actionSet.ActionOptions,
                        buttons: t.creative.Options.buttons,
                        buttonStyles: QSI.CreativeManager.SocialMediaHelper.getButtonStyles(t.creative),
                        displayOptions: t.creative.Options.displayOptions,
                        insertionLocation: t.actionSet.ActionOptions.displayElement ? t.actionSet.ActionOptions.displayElement : t.id
                    });
                    QSI.reg[t.interceptID] = e
                }
            }()
        },
        runUserDefinedHTML: function(n) {
            ! function() {
                var e = {
                    id: n.interceptID,
                    type: QSI.util.creativeTypes.USER_DEFINED_HTML,
                    targetURL: n.decision.Target.URL,
                    targetURLOrigin: n.decision.Target.OriginalURLOrigin,
                    impressionURL: QSI.CreativeManager.Utilities.getImpressionURL({
                        interceptID: n.interceptID,
                        creativeID: n.decision.Creative.ID,
                        asid: n.decision.ActionSetID
                    }),
                    interceptDisplayOptions: n.intercept.DisplayOptions,
                    actionOptions: n.actionSet.ActionOptions,
                    displayOptions: n.creative.Options.displayOptions,
                    insertionLocation: n.actionSet.ActionOptions.displayElement ? n.actionSet.ActionOptions.displayElement : n.id
                };
                if (n.creative.Options.elements) e.elements = n.creative.Options.elements, e.resetStyle = QSI.CreativeManager.Utilities.getResetStyle("QSIUserDefinedHTML");
                else {
                    e.html = n.creative.Options.html, e.size = n.creative.Options.size;
                    var t = QSI.CreativeManager.Utilities.parsePipedText(n.creative.Options.html);
                    t && 0 < t.length && (e.locators = t)
                }
                var i = new QSI.UserDefinedHTML(e);
                QSI.reg[n.interceptID] = i
            }()
        }
    }, QSI.CreativeManager.SocialMediaHelper = {
        getButtonStyles: function(e) {
            var t = e.Options.displayOptions.size;
            e.Options.displayOptions.useWidget && (t = "Large" === t ? "80px" : "");
            var i = QSI.CreativeManager.SocialMediaHelper.getThemeStyles(e),
                n = QSI.CreativeManager.SocialMediaHelper.getPadding(t),
                r = QSI.CreativeManager.SocialMediaHelper.getBoxShadow(e, t),
                a = QSI.CreativeManager.SocialMediaHelper.getWidth(e, t),
                o = ".QSI_SocialMediaButton { float: left; cursor: pointer; background: " + i.background + "; border-bottom: " + i.border + "; border-right: " + i.border + "; border-top: " + i.highlight + "; border-left: " + i.highlight + "; } #QSI_SocialMediaContainer { border-top: " + i.border + "; border-left: " + i.border + "; box-shadow: " + r.shadowOutset + "; border-radius: " + r.borderRadius + "; } ";
            return e.Options.displayOptions.useWidget ? (o += QSI.CreativeManager.SocialMediaHelper.getWidgetStyles(e, t), "Large" === t && (o += "#QSI_SocialMediaContainer { width: " + a + "; } ")) : o += "#QSI_SocialMediaContainer { width: " + a + "; } .QSI_SocialMediaButton { padding: " + n + "; width: " + t + "; height: " + t + "; } ", 1 === QSI.CreativeManager.SocialMediaHelper.getMaxCols(e) ? o += ".QSI_First .QSI_SocialMediaButton { border-radius: " + r.firstBorderRadius + "; } .QSI_Last .QSI_SocialMediaButton { border-radius: " + r.lastBorderRadius + "; } " : o += ".QSI_First .QSI_SocialMediaButton.QSI_First { border-radius: " + r.tlBorderRadius + "; } QSI_First .QSI_SocialMediaButton.QSI_Last { border-radius: " + r.trBorderRadius + "; } QSI_Last .QSI_SocialMediaButton.QSI_First { border-radius: " + r.blBorderRadius + "; } QSI_Last .QSI_SocialMediaButton.QSI_Last { border-radius: " + r.brBorderRadius + "; } ", o
        },
        getThemeStyles: function(e) {
            var t = {
                background: "none",
                border: "none",
                highlight: "none"
            };
            switch (e.Options.displayOptions.theme) {
                case "Transparent":
                    break;
                case "Glass":
                    if (!("Internet Explorer" === QSI.Browser.name && QSI.Browser.version < 8)) {
                        t.background = "rgba(255, 255, 255, .4)", t.border = "1px solid rgba(200, 200, 200, .6)", t.highlight = "1px solid rgba(255, 255, 255, .8)";
                        break
                    }
                    case "Light":
                        t.background = "#E6E6E6", t.border = "1px solid #B3B3B3", t.highlight = "1px solid #FFFFFF";
                        break;
                    case "Medium":
                        t.background = "#666666", t.border = "1px solid #4D4D4D", t.highlight = "1px solid #7D7D7D";
                        break;
                    case "Dark":
                        t.background = "#333333", t.border = "1px solid #1A1A1A", t.highlight = "1px solid #484848"
            }
            return t
        },
        getPadding: function(e) {
            var t = "";
            switch (e) {
                case "64px":
                    t = "8px";
                    break;
                case "48px":
                    t = "7px";
                    break;
                case "32px":
                    t = "6px";
                    break;
                case "24px":
                    t = "5px"
            }
            return t
        },
        getBoxShadow: function(e, t) {
            var i = "0",
                n = {};
            if ("Small" === t) return {
                shadowOutset: "none",
                shadowInset: "none",
                borderRadius: "none",
                tlBorderRadius: "none",
                trBorderRadius: "none",
                blBorderRadius: "none",
                brBorderRadius: "none",
                firstBorderRadius: "none",
                lastBorderRadius: "none"
            };
            switch (e.Options.displayOptions.theme) {
                case "Transparent":
                    break;
                case "Glass":
                    i = .15;
                    break;
                case "Light":
                    i = .25;
                    break;
                case "Medium":
                    i = .45;
                    break;
                case "Dark":
                    i = .6
            }
            n.shadowOutset = "0px 0px 10px 0px rgba(0, 0, 0, " + i + ")", n.shadowInset = "none";
            var r = "3px";
            switch (t) {
                case "64px":
                    r = "6px";
                    break;
                case "48px":
                    r = "5px";
                    break;
                case "32px":
                    r = "4px";
                    break;
                case "24px":
                    r = "3px"
            }
            if (n.borderRadius = r, n.tlBorderRadius = r + " 0px 0px 0px", n.trBorderRadius = "0px " + r + " 0px 0px", n.blBorderRadius = "0px 0px 0px " + r, n.brBorderRadius = "0px 0px " + r + " 0px", n.firstBorderRadius = r + " " + r + " 0px 0px", n.lastBorderRadius = "0px 0px " + r + " " + r, 0 === e.Options.displayOptions.xOffset) switch (e.Options.displayOptions.position) {
                case "OTLL":
                case "OML":
                case "OBLL":
                    n.shadowOutset = "0px -4px 10px -2px rgba(0, 0, 0, " + i + "), -5px 0px 10px -4px rgba(0, 0, 0, " + i + "), -2px 4px 10px -3px rgba(0, 0, 0, " + i + ")", n.borderRadius = r + " 0px 0px " + r, n.tlBorderRadius = r + " 0px 0px " + r, n.trBorderRadius = "none", n.blBorderRadius = "0px 0px 0px " + r, n.brBorderRadius = "none", n.firstBorderRadius = r + " 0px 0px 0px", n.lastBorderRadius = "0px 0px 0px " + r;
                    break;
                case "OTRR":
                case "OMR":
                case "OBRR":
                    n.shadowOutset = "2px -5px 10px -3px rgba(0, 0, 0, " + i + "), 8px 0px 10px -6px rgba(0, 0, 0, " + i + "), 0px 5px 10px -3px rgba(0, 0, 0, " + i + ")", n.borderRadius = "0px " + r + " " + r + " 0px", n.tlBorderRadius = "none", n.trBorderRadius = "0px " + r + " 0px 0px", n.blBorderRadius = "none", n.brBorderRadius = "0px 0px " + r + " 0px", n.firstBorderRadius = "0px " + r + " 0px 0px", n.lastBorderRadius = "0px 0px " + r + "0px"
            }
            return n
        },
        getWidth: function(e, t) {
            var i = 0;
            switch (t) {
                case "64px":
                    i = 82;
                    break;
                case "48px":
                    i = 64;
                    break;
                case "32px":
                    i = 46;
                    break;
                case "24px":
                    i = 38;
                    break;
                case "Large":
                case "Small":
                    i = 82
            }
            return i * QSI.CreativeManager.SocialMediaHelper.getMaxCols(e)
        },
        getMaxCols: function(e) {
            var t = 0;
            for (var i in e.Options.buttons) Object.prototype.hasOwnProperty.call(e.Options.buttons, i) && (t = Math.max(t, e.Options.buttons[i].length));
            return t
        },
        getWidgetStyles: function(e, t) {
            var i = ".QSI_SocialMediaButton {position:relative;} ";
            for (var n in i += "Small" === t ? ".QSI_SocialMediaButton .Content { height: 20px; padding: 3px; } " : ".QSI_SocialMediaButton { height: 80px; width: 80px; } ", e.Options.buttons)
                if (Object.prototype.hasOwnProperty.call(e.Options.buttons, n)) {
                    var r = e.Options.buttons[n];
                    if (r) switch (r.type) {
                        case "Facebook":
                            i += "Large" === t ? ".QSI_SocialMediaButton.QSI_Facebook .Content { width: 48px; height: 62px; position: absolute; top: 9px; left: 17px; } " : ".QSI_SocialMediaButton.QSI_Facebook .Content { width: 90px; } ";
                            break;
                        case "GooglePlus":
                            i += "Large" === t ? ".QSI_SocialMediaButton.QSI_GooglePlus .Content { width: 50px; height: 60px; position: absolute; top: 11px; left: 16px; } " : ".QSI_SocialMediaButton.QSI_GooglePlus .Content { width: 90px; } ";
                            break;
                        case "Twitter":
                            i += "Large" === t ? ".QSI_SocialMediaButton.QSI_Twitter .Content { width: 55px; height: 62px; position: absolute; top: 10px; left: 13px; } " : ".QSI_SocialMediaButton.QSI_Twitter .Content { width: 107px; } ";
                            break;
                        case "LinkedIn":
                            i += "Large" === t ? ".QSI_SocialMediaButton.QSI_LinkedIn .Content { height: 62px; position: absolute; top: 9px; left: 11px; } " : ".QSI_SocialMediaButton.QSI_LinkedIn .Content { width: 93px; } ";
                            break;
                        case "Reddit":
                            i += "Large" === t ? ".QSI_SocialMediaButton.QSI_Reddit .Content { width: 50px; height: 66px; position: absolute; top: 5px; left: 15px; } " : ".QSI_SocialMediaButton.QSI_Reddit .Content { height: 17px; width: 75px; margin-top: 3px; } ";
                            break;
                        case "Digg":
                            i += "Large" === t ? ".QSI_SocialMediaButton.QSI_Digg .Content { width: 54px; height: 69px; position: absolute; top: 6px; left: 14px; } " : ".QSI_SocialMediaButton.QSI_Digg .Content { width: 76px; } "
                    }
                } return i
        }
    }, QSI.CreativeManager.Utilities = {
        getImpressionURL: function(e) {
            var t = {
                Q_Impress: 1,
                Q_CID: e.creativeID,
                Q_SIID: e.interceptID,
                Q_ASID: e.asid,
                Q_CLIENTVERSION: QSI.global.clientVersion || "unknown",
                Q_CLIENTTYPE: QSI.global.clientType || "unknown"
            };
            return QSI.baseURL + "?" + QSI.Orchestrator.generateQueryString(t)
        },
        getWebResponsiveResetStyle: function(a) {
            var t = "";
            return [{
                selectorList: ["div", "dl", "dt", "dd", "ul", "ol", "li", "h1", "h2", "h3", "h4", "h5", "h6", "span", "pre", "form", "fieldset", "textarea", "p", "blockquote", "tr", "th", "td"],
                styleResets: "{ margin: 0; padding: 0;background-color: transparent; border: 0; font-size: 12px; line-height: normal; vertical-align:baseline; box-shadow: none; }"
            }, {
                selectorList: ["img"],
                styleResets: "{ height: auto; width: auto; margin: 0; padding: 0 }"
            }, {
                selectorList: ["ul", "ol"],
                styleResets: "{ margin: 12px 0; padding-left: 40px; }"
            }, {
                selectorList: ["ul li"],
                styleResets: "{ list-style-type: disc; }"
            }, {
                selectorList: ["ol li"],
                styleResets: "{ list-style-type: decimal; }"
            }, {
                selectorList: [".scrollable"],
                styleResets: "{ -webkit-overflow-scrolling: touch; }"
            }, {
                selectorList: ["table"],
                styleResets: "{ border-collapse: collapse; border-spacing: 0; }"
            }, {
                selectorList: ["table td"],
                styleResets: "{ padding: 2px; }"
            }, {
                selectorList: ["*"],
                styleResets: "{ box-sizing: content-box; }"
            }].forEach(function(e) {
                t += function(e, t) {
                    if (0 === e.length) return "";
                    for (var i = "." + a, n = "", r = 0; r < e.length; r++) n += i + " " + e[r] + ",";
                    return (n = n.slice(0, -1)) + t
                }(e.selectorList, e.styleResets)
            }), t
        },
        getResetStyle: function(e) {
            var t = "." + e;
            return t + " div," + t + " dl," + t + " dt," + t + " dd," + t + " ul," + t + " ol," + t + " li," + t + " h1," + t + " h2," + t + " h3," + t + " h4," + t + " h5," + t + " h6," + t + " pre," + t + " form," + t + " fieldset," + t + " textarea," + t + " p," + t + " blockquote," + t + " th," + t + " td {margin: 0;padding: 0;color: black;font-family: arial;font-size: 12px;line-height: normal;}" + t + " ul {margin: 12px 0;padding-left: 40px;}" + t + " ol," + t + " ul {margin: 12px 0;padding-left: 40px;}" + t + " ul li {list-style-type: disc;}" + t + " ol li {list-style-type: decimal;}" + t + " .scrollable {-webkit-overflow-scrolling: touch;}" + t + " table {border-collapse: collapse;border-spacing: 0;}" + t + " table td {padding: 2px;}.QSIPopOver *,.QSISlider *,.QSIPopUnder *,.QSIEmbeddedTarget * {box-sizing: content-box;}"
        },
        hasCreativeEmbeddedTarget: function(e) {
            if (!e || !e.Options.elements || !e.Options.elements.Elements) return !1;
            var t = e.Options.elements.Elements;
            for (var i in t)
                if (Object.prototype.hasOwnProperty.call(t, i)) {
                    var n = t[i];
                    if (n.type && "EmbeddedTarget" === n.type) return !0
                } return !1
        },
        parsePipedText: function(e) {
            for (var t, i = /\$\{(SI)?([A-Za-z]*):\/\/([^\}]*)\}/g, n = [];
                (t = i.exec(e)) && n.push(t), t;);
            return n
        }
    }, QSI.Orchestrator = {
        init: function() {
            if (this.setupJFEMessageEventHandlerForIOSOptimization(), QSI.global.latency = {
                    si: {
                        metricName: "si.SILatency",
                        latencyStartTime: null,
                        requestURL: null,
                        componentLatencies: {},
                        targetingRequestStartTime: null,
                        targetingRequestEndTime: null,
                        coreModuleLatencyStartTime: null,
                        coreModuleLatencyEndTime: null
                    },
                    siDPR: {
                        metricName: "si.SIDPRLatency",
                        latencyStartTime: null,
                        requestURL: null,
                        componentLatencies: {},
                        dprRequestStartTime: null,
                        dprRequestEndTime: null
                    }
                }, QSI.global.latencySamplePercentage = .02, QSI.LoadingState || (QSI.LoadingState = []), QSI.PendingQueue || (QSI.PendingQueue = []), QSI.global.latency.si.latencyStartTime = Date.now(), QSI.global.legacyId) {
                QSI.Request || (QSI.Request = {}), QSI.debug = {};
                var e, t = QSI.global.legacyId,
                    i = {
                        id: t
                    };
                if (0 === t.indexOf("ZN") ? i.ZoneID = t : i.InterceptID = t, QSI.global.isHostedJS()) void 0 === (e = QSI.Orchestrator.parseQueryString(window.location.href)).Q_DEBUG && !QSI.config.debug || (i.Q_DEBUG = null, QSI.initFullDbg()), !QSI.config.editing && "0" !== QSI.global.version || (i.version = "0");
                else {
                    var n;
                    if (document.currentScript) n = document.currentScript.src;
                    else try {
                        var r = document.querySelectorAll("script"),
                            a = [];
                        for (var o in r) Object.prototype.hasOwnProperty.call(r, o) && (a[o] = r[o]);
                        var s = QSI.global.baseURL;
                        0 === s.indexOf("https://") ? s = s.substring(8) : 0 === s.indexOf("http://") ? s = s.substring(7) : 0 === s.indexOf("//") && (s = s.substring(2)), n = a.filter(function(e) {
                            return (-1 !== e.src.indexOf(s + "/WRSiteInterceptEngine/?") || -1 !== e.src.indexOf(s + "/SIE/?")) && (-1 === e.src.indexOf("Q_Impress") && -1 === e.src.indexOf("Q_Redirect") && -1 === e.src.indexOf("Q_Click") && -1 === e.src.indexOf("Q_DPR"))
                        })[0].src
                    } catch (e) {
                        QSI.dbg.e("An error occurred while loading the intercept.", e)
                    }
                    void 0 !== (e = QSI.Orchestrator.parseQueryString(n)).Q_NOCACHE && (i.Q_NOCACHE = null), void 0 !== e.Q_BOOKMARKLET && (i.Q_BOOKMARKLET = null, i.Q_DEBUG = null, QSI.initFullDbg()), void 0 !== e.Q_DEBUG && (i.Q_DEBUG = null, QSI.initFullDbg()), void 0 !== e.Q_VERSION && (i.version = e.Q_VERSION)
                }
                void 0 !== QSI.global.clientVersion && null !== QSI.global.clientVersion && (i.Q_CLIENTVERSION = QSI.global.clientVersion), void 0 !== QSI.global.clientType && null !== QSI.global.clientType && (i.Q_CLIENTTYPE = QSI.global.clientType), QSI.Orchestrator.load(i)
            } else if (!QSI.Request) {
                QSI.Request = {};
                for (var c = document.querySelectorAll("[data-siteinterceptscript]"), l = 0; l < c.length; l++) {
                    var p = c[l];
                    QSI.isDebug = QSI.isDebug || p.hasAttribute("data-qdebug") || -1 !== window.location.href.indexOf("Q_DEBUG"), QSI.isDebug && QSI.initFullDbg();
                    var d = {};
                    p.hasAttribute("data-interceptid") && (d.InterceptID = p.getAttribute("data-interceptid"), d.id = d.InterceptID), p.hasAttribute("data-zoneid") && (d.ZoneID = p.getAttribute("data-zoneid"), d.id = d.ZoneID), p.hasAttribute("data-qnocache") && (d.Q_NOCACHE = null), p.hasAttribute("data-qbookmarklet") && (d.Q_BOOKMARKLET = null), QSI.isDebug && (d.Q_DEBUG = null), p.hasAttribute("data-version") && (d.version = p.getAttribute("data-version")), QSI.Orchestrator.load(d)
                }
            }
        },
        load: function(t) {
            QSI.LoadingState.push(!0), QSI.global.latency.si.requestURL = QSI.Orchestrator.generateTargetingURL(t), QSI.global.latency.si.targetingRequestStartTime = Date.now();
            var e = QSI.Orchestrator.getQLOCPostData(),
                i = QSI.AssetManager.promiseFetch("POST", QSI.global.latency.si.requestURL, e);
            QSI.Request[t.id] = {}, QSI.Request[t.id].Intercepts = {}, QSI.Request[t.id].Params = t, i.then(function() {
                QSI.global.latency.si.targetingRequestEndTime = Date.now(), QSI.global.latency.si.componentLatencies.targetingRequest = QSI.global.latency.si.targetingRequestEndTime - QSI.global.latency.si.targetingRequestStartTime
            }).then(QSI.Orchestrator.handleTargetingResponse.bind(null, t), function(e) {
                QSI.dbg.e((e.Message, e.Message)), t.deferred && t.deferred.reject()
            })
        },
        getQLOCPostData: function() {
            return QSI.shouldStripQueryParamsInQLoc ? "Q_LOC=" + encodeURIComponent(window.location.href.split("?")[0]) : "Q_LOC=" + encodeURIComponent(window.location.href)
        },
        generateTargetingURL: function(e) {
            var t = QSI.baseURL + "Targeting.php?",
                i = [];
            e.InterceptID && i.push("Q_InterceptID=" + e.InterceptID), e.ZoneID && i.push("Q_ZoneID=" + e.ZoneID), null === e.Q_DEBUG && (i.push("Q_DEBUG"), QSI.isDebug = !0), null === e.Q_BOOKMARKLET && i.push("Q_BOOKMARKLET"), null === e.Q_NOCACHE && i.push("Q_NOCACHE"), void 0 !== e.version && null !== e.version && i.push("Version=" + e.version);
            var n = QSI.profile.get("QualtricsSurveyHistory", "", 1);
            if (n) {
                var r = encodeURIComponent(Object.keys(n));
                i.push("Q_QualtricsSurveyTaken=" + r)
            }
            return void 0 !== e.Q_CLIENTVERSION && null !== e.Q_CLIENTVERSION && i.push("Q_CLIENTVERSION=" + e.Q_CLIENTVERSION), void 0 !== e.Q_CLIENTTYPE && null !== e.Q_CLIENTTYPE && i.push("Q_CLIENTTYPE=" + e.Q_CLIENTTYPE), t + i.join("&")
        },
        isMessageEventOriginAllowed: function(e) {
            if (QSI.reg && QSI.reg instanceof Object)
                for (var t in QSI.reg)
                    if (Object.prototype.hasOwnProperty.call(QSI.reg, t) && QSI.reg[t] && QSI.reg[t].options && QSI.reg[t].options.targetURLOrigin && QSI.reg[t].options.targetURLOrigin === e) return !0;
            return QSI.dbg && QSI.dbg.c && QSI.dbg.c("Event origin is not allowed: " + e), !1
        },
        handleTargetingResponse: function(i, t) {
            try {
                var n = "";
                if ("SampleRejected" === t) return;
                try {
                    n = JSON.parse(t)
                } catch (e) {
                    return void QSI.dbg.e("Failed to parse JSON of targeting response: " + t)
                }
                if (n.Error) return void QSI.dbg.e(n.Message);
                QSI.Orchestrator.setGlobalVars(n), QSI.Orchestrator.setupDBGLog();
                var e = n.Modules;
                if (e.Core) QSI.AssetManager.promiseLoadScript("Core", e.Core).then(function() {
                    null === QSI.global.latency.si.coreModuleLatencyEndTime && (QSI.global.latency.si.coreModuleLatencyEndTime = Date.now(), QSI.global.latency.si.componentLatencies.coreModuleRequest = QSI.global.latency.si.coreModuleLatencyEndTime - QSI.global.latency.si.coreModuleLatencyStartTime);
                    try {
                        if (QSI.history.logVisit(), n.Intercepts)
                            for (var e = 0; e < n.Intercepts.length; e++) {
                                var t = n.Intercepts[e];
                                t && (t.Error ? QSI.dbg.log(t.Message) : (QSI.Request[i.id].Intercepts[t.InterceptID] = {}, QSI.Request[i.id].Intercepts[t.InterceptID].Targeting = t))
                            }!QSI.Request[i.id].hasDependencies && QSI.Request[i.id].hasBeenResolved || (QSI.EventTracker.trackElements(), QSI.EventTracker.incrementEventList(), window._qsie = QSI.API.Events), n.Dependencies ? (QSI.global.latency.siDPR.latencyStartTime = Date.now(), QSI.Orchestrator.handleDependencyResolver(i, n)) : QSI.Orchestrator.loadModules(i, n)
                    } catch (e) {
                        QSI.dbg.e(e)
                    }
                }, function() {});
                else QSI.Orchestrator.doneLoading(i)
            } catch (e) {
                QSI.dbg.e(e)
            }
        },
        setupDBGLog: function() {
            QSI.dbg && (QSI.dbg.log = function(e, t, i) {
                if (QSI.ClientLog)
                    if (QSI.global.featureFlags.isClientLoggingEnabled) QSI.ClientLog.send(e, t, i);
                    else {
                        QSI.ClientLog.sampledSend(e, t, i, .02)
                    }
            })
        },
        handleDependencyResolver: function(t, e) {
            var i = QSI.Orchestrator.generateTargetingURL(t);
            i += "&t=" + (new Date).getTime(), i += "&Q_VSI=" + encodeURIComponent(JSON.stringify(e.RequestData.validIntercepts)), i += "&Q_DPR=true", QSI.global.latency.siDPR.requestURL = i, QSI.global.latency.siDPR.dprRequestStartTime = Date.now();
            var n = "";
            for (var r in e.Dependencies) Object.prototype.hasOwnProperty.call(e.Dependencies, r) && ("SiteCatalyst" === r && (QSI["Resolve" + r].rootName = QSI.adobeVar), n += QSI["Resolve" + r].prepare(e.Dependencies[r]));
            n += "&" + QSI.Orchestrator.getQLOCPostData(), QSI.AssetManager.promiseFetch("POST", i, n).then(function() {
                QSI.global.latency.siDPR.dprRequestEndTime = Date.now(), QSI.global.latency.siDPR.componentLatencies.dprRequest = QSI.global.latency.siDPR.dprRequestEndTime - QSI.global.latency.siDPR.dprRequestStartTime
            }).then(QSI.Orchestrator.handleTargetingResponse.bind(this, t), function(e) {
                QSI.dbg.e((e.Message, e.Message)), t.deferred && t.deferred.reject()
            })
        },
        loadModules: function(i, e) {
            var t = [],
                n = e.Modules;
            for (var r in n)
                if (Object.prototype.hasOwnProperty.call(n, r)) {
                    if ("ScreenCapture" === r) {
                        QSI.Orchestrator.setupScreenCaptureListener(n[r]);
                        continue
                    }
                    if (("HTTPRedirect" === r || "SocialMedia" === r || "LatencyLog" === r) && QSI.global.isHostedJS()) {
                        QSI.dbg.c("Module type '" + r + "' is not supported when using Site Intercept Hosted JS");
                        continue
                    }
                    var a = QSI.AssetManager.promiseLoadScript(r, n[r]);
                    t.push(a)
                } for (var o = 0; o < e.Intercepts.length; o++) {
                var s = e.Intercepts[o];
                if (s && !s.Error && null !== s.Decision.ActionSetID) {
                    var c = QSI.AssetManager.promiseLoadIntercept(i.id, s, i),
                        l = QSI.AssetManager.promiseLoadCreative(i.id, s, i);
                    if (t.push(c, l), s.Decision.PopUnderTarget && s.Decision.PopUnderTarget.ID && "Target" !== s.Decision.PopUnderTarget.ID) {
                        var p = QSI.AssetManager.promiseLoadPopUnderTarget(i.id, s, i);
                        t.push(p)
                    }
                }
            }
            QSI.Orchestrator.when.apply(this, t).then(function() {
                try {
                    if (QSI.global.featureFlags["DX.ContactFrequency"]) QSI.ContactFrequency.checkContactFrequencyRules(QSI.Request[i.id].Intercepts, QSI.global.brandID).then(function() {
                        QSI.Orchestrator.prepareIntercepts(i, e)
                    });
                    else QSI.Orchestrator.prepareIntercepts(i, e)
                } catch (e) {
                    QSI.dbg.e(e)
                }
            }, function(e) {
                var t = "";
                "string" == typeof e ? t = e : e && "string" == typeof e.Message && (t = e), QSI.dbg.e("An error occurred while loading the intercept. " + t), i.deferred && i.deferred.reject()
            })
        },
        setInterceptDisplayOptionCallback: function(e, t) {
            switch (e) {
                case "":
                case "onfocus":
                    QSI.util.waitForFocus().done(t);
                    break;
                case "onload":
                    t();
                    break;
                case "onexit":
                    QSI.util.waitForExitIntent().done(t);
                    break;
                default:
                    return
            }
        },
        prepareIntercepts: function(e, t) {
            QSI.global.featureFlags.isLatencyLoggingEnabled && QSI.global.latency && !0 !== e.loadingFromAPI && [QSI.global.latency.si, QSI.global.latency.siDPR].forEach(function(e) {
                if (e && "number" == typeof e.latencyStartTime) {
                    var t = Date.now() - e.latencyStartTime;
                    QSI.LatencyLog.sampledSend(QSI.global.latencySamplePercentage, e.metricName, t, e.componentLatencies, {
                        requestURL: e.requestURL
                    })
                }
            });
            QSI.Orchestrator.setGlobalIncludes(e, t), ("manual" !== QSI.Request[e.id].displayInterceptType && !QSI.Request[e.id].zoneManualDisplay || QSI.Request[e.id].hasDependencies) && QSI.RunIntercepts(e.id, !1), QSI.Orchestrator.doneLoading(e), e.deferred && e.deferred.resolve()
        },
        setGlobalIncludes: function(e, t) {
            QSI.isDebug && (QSI.Request[e.id].Debug = {
                debugInfo: JSON.parse(t.DebugInfo),
                version: t.RequestData.bVersion
            }, QSI.debug.version = t.RequestData.bVersion, QSI.debug.debugInfo = JSON.parse(t.DebugInfo)), QSI.version = t.RequestData.bVersion, QSI.InterceptsRan = !1, QSI.Request[e.id].hasDependencies = t.RequestData.hasDependencies, QSI.Request[e.id].hasBeenResolved = t.RequestData.hasBeenResolved, QSI.Request[e.id].displayInterceptType = t.RequestData.displayInterceptType, QSI.Request[e.id].zoneManualDisplay = t.RequestData.zoneManualDisplay
        },
        setGlobalVars: function(e) {
            QSI.Browser = {
                    name: e.RequestData.browser,
                    version: e.RequestData.browserVersion,
                    isMobile: e.RequestData.isMobile,
                    isBrowserSupported: e.RequestData.isBrowserSupported
                }, QSI.CORSOrigin = e.RequestData.CORSOrigin, QSI.OS = {
                    name: e.RequestData.osName,
                    version: e.RequestData.osVersion
                }, QSI.global.brandID = e.RequestData.brandID, QSI.global.brandDC = e.RequestData.brandDC, QSI.global.isHostedJS() ? (QSI.global.graphicPath = "https://" + QSI.global.brandDC + "/WRQualtricsSiteIntercept/Graphic.php?IM=", QSI.global.imagePath = "https:" + QSI.global.baseURL + "/WRSiteInterceptEngine/../WRQualtricsShared/Graphics/") : (QSI.global.graphicPath = "https://" + QSI.global.brandDC + "/WRQualtricsSiteIntercept/Graphic.php?IM=", QSI.global.imagePath = QSI.global.baseURL + "/WRQualtricsShared/Graphics/"), QSI.global.maxCookieSize = e.RequestData.maxCookieSize, QSI.global.featureFlags = e.FeatureFlags, QSI.global.screenCaptureServiceBaseURL = e.RequestData.screenCaptureServiceBaseURL, QSI.global.eventTrackers = e.RequestData.eventTrackers, QSI.adobeVar = e.RequestData.adobeSCVariable, QSI.id = e.RequestData.ID, QSI.reqID[e.RequestData.ID] = !0, QSI.CookieDomain = e.RequestData.cookieDomain, QSI.historyStorageType = e.RequestData.historyStorageType, QSI.historyStorageSize = e.RequestData.historyStorageSize, QSI.currentURL = window.location.href.split("?")[0],
                function() {
                    var e = document.createElement("div");
                    e.className = "scrollbar-measure", e.style.width = "100px", e.style.height = "100px", e.style.overflow = "scroll", e.style.position = "absolute", e.style.top = "-99999px", document.body.appendChild(e);
                    var t = e.offsetWidth - e.clientWidth;
                    QSI.scrollbarWidth = t, document.body.removeChild(e)
                }()
        },
        parseQueryString: function(e) {
            var t = {};
            if (e && -1 !== e.indexOf("?")) {
                var i = e.split("?", 2)[1];
                i = i.split("&");
                for (var n = 0; n < i.length; n++) {
                    var r = i[n].split("=", 2);
                    "Q_LOC" === r[0] && -1 !== r[1].indexOf("Q_DEBUG") && (t.Q_DEBUG = !0), r[0] && (t[r[0]] = decodeURIComponent(r[1]))
                }
            }
            return t
        },
        generateQueryString: function(e) {
            var t = [];
            for (var i in e)
                if (Object.prototype.hasOwnProperty.call(e, i)) {
                    var n = i;
                    e[i] && (n += "=" + encodeURIComponent(e[i])), t.push(n)
                } return t.join("&")
        },
        getClientVersionQueryString: function() {
            return QSI.Orchestrator.generateQueryString({
                Q_CLIENTVERSION: QSI.global.clientVersion || "unknown",
                Q_CLIENTTYPE: QSI.global.clientType || "unknown"
            })
        },
        replaceAll: function(e, t, i) {
            return t = t.replace(/([.*+?^${}()|\[\]\\=!:\/])/g, "\\$1"), e.replace(new RegExp(t, "g"), i)
        },
        Deferred: function() {
            var i = {},
                n = "pending",
                r = [],
                a = [],
                o = [],
                s = [],
                c = {
                    state: function() {
                        return n
                    },
                    then: function(e, t) {
                        return this.done(e).fail(t), this
                    },
                    done: function(e) {
                        if ("pending" === n) o.push(e);
                        else if ("resolved" === n) try {
                            e.apply(this, r)
                        } catch (e) {
                            "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                        }
                        return this
                    },
                    fail: function(e) {
                        if ("pending" === n) s.push(e);
                        else if ("rejected" === n) try {
                            e.apply(this, a)
                        } catch (e) {
                            "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                        }
                        return this
                    },
                    promise: function() {
                        return c
                    }
                };
            return QSI.Orchestrator.forOwn(c, function(e, t) {
                i[t] = c[t]
            }), i.resolve = function() {
                if ("pending" === n) {
                    n = "resolved";
                    var t = QSI.Orchestrator.createArrayFromArguments(arguments);
                    r = t;
                    var i = this;
                    QSI.Orchestrator.each(o, function(e) {
                        try {
                            e.apply(i, t)
                        } catch (e) {
                            "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                        }
                    })
                }
            }, i.reject = function() {
                if ("pending" === n) {
                    n = "rejected";
                    var t = QSI.Orchestrator.createArrayFromArguments(arguments);
                    a = t;
                    var i = this;
                    QSI.Orchestrator.each(s, function(e) {
                        try {
                            e.apply(i, t)
                        } catch (e) {
                            "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                        }
                    })
                }
            }, i
        },
        when: function(e) {
            function t(t, i) {
                return function(e) {
                    i[t] = 1 < arguments.length ? e : QSI.Orchestrator.createArrayFromArguments(arguments), --r || a.resolve(i)
                }
            }
            var i = QSI.Orchestrator.createArrayFromArguments(arguments),
                n = i.length,
                r = n,
                a = 1 === r ? e : QSI.Orchestrator.Deferred();
            if (1 < n)
                for (var o = 0; o < n; o++) i[o] && i[o].promise ? i[o].promise().done(t(o, i)).fail(a.reject) : r--;
            return r < 1 && a.resolve(i), a.promise()
        },
        createArrayFromArguments: function(e) {
            return e ? Array.prototype.slice.call(e) : []
        },
        isFunction: function(e) {
            return "function" == typeof e || !1
        },
        forOwn: function(e, t) {
            if (e && e instanceof Object && this.isFunction(t))
                for (var i in e) Object.prototype.hasOwnProperty.call(e, i) && t(e[i], i, e)
        },
        each: function(e, t) {
            var i = e.length;
            if (i)
                for (var n = 0; n < i; n++) t(e[n], n)
        },
        doneLoading: function(e) {
            if (QSI.LoadingState.pop(), 0 === QSI.LoadingState.length) {
                if (!0 !== e.loadingFromAPI) {
                    var t = document.createEvent("Event");
                    t.initEvent("qsi_js_loaded", !0, !0), window.dispatchEvent(t)
                }
                for (var i = QSI.PendingQueue.length, n = 0; n < i; n++) {
                    QSI.PendingQueue.shift()()
                }
            }
        },
        setupJFEMessageEventHandlerForIOSOptimization: function() {
            if (!QSI.JFEListenerRegistered) {
                QSI.JFEListenerRegistered = !0;
                return window.addEventListener("message", function(e) {
                    try {
                        if (QSI.Orchestrator && QSI.Orchestrator.isMessageEventOriginAllowed && !QSI.Orchestrator.isMessageEventOriginAllowed(e.origin)) return;
                        ! function(e) {
                            var t = null;
                            if ("string" == typeof e) try {
                                t = JSON.parse(e)
                            } catch (e) {
                                return !1
                            }
                            return null !== t && "JFE" === t.from && "SI" === t.to && "JFELoaded" === t.event && "iOS" === QSI.OS.name
                        }(e.data) || function(e, t) {
                            e.postMessage({
                                event: "addIOSSIWorkaround",
                                from: "SI",
                                to: "JFE"
                            }, t)
                        }(e.source, e.origin)
                    } catch (e) {
                        "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                    }
                })
            }
        },
        setupScreenCaptureListener: function(n) {
            if (!QSI.screenCaptureListenerRegistered) return QSI.screenCaptureListenerRegistered = !0, window.addEventListener("message", function(e) {
                try {
                    if (QSI.Orchestrator && QSI.Orchestrator.isMessageEventOriginAllowed && !QSI.Orchestrator.isMessageEventOriginAllowed(e.origin)) return;
                    var t = QSI.util.getOriginInterceptOfMessage(e.source);
                    if (!t) return;
                    var i = e.data;
                    if ("string" == typeof i) try {
                        i = JSON.parse(i)
                    } catch (e) {
                        return
                    }
                    if (!i || "JFE" !== i.from || "SI" !== i.to) return;
                    if (QSI.screenCaptureHandlers || (QSI.screenCaptureHandlers = {}), "canScreenCapture" === i.event) {
                        void 0 === QSI.screenCaptureModulePromise && (QSI.screenCaptureModulePromise = QSI.AssetManager.promiseLoadScript("ScreenCapture", n));
                        return void e.source.postMessage(JSON.stringify({
                            event: "canScreenCapture",
                            from: "SI",
                            to: "JFE",
                            canScreenCapture: !0
                        }), e.origin)
                    }
                    void 0 !== QSI.screenCaptureModulePromise && QSI.screenCaptureModulePromise.then(function() {
                        switch (QSI.screenCaptureHandlers[i.sessionId] || (QSI.screenCaptureHandlers[i.sessionId] = new QSI.ScreenCaptureHandler(t, e.source, i.sessionId, i.translations, e.origin)), i.event) {
                            case "startScreenCapture":
                                QSI.screenCaptureHandlers[i.sessionId].captureScreen(i.questionId);
                                break;
                            case "editScreenCapture":
                                QSI.screenCaptureHandlers[i.sessionId].editAnnotations(i.questionId);
                                break;
                            case "removeScreenCapture":
                                QSI.screenCaptureHandlers[i.sessionId].removeScreenCapture(i.questionId);
                                break;
                            case "sessionFinished":
                                QSI.screenCaptureHandlers[i.sessionId].removeAllScreenCaptures();
                                break;
                            default:
                                return
                        }
                    })
                } catch (e) {
                    "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
                }
            })
        }
    }, QSI.RunIntercepts = function(e, t) {
        void 0 === e && (e = null), QSI.util.observe(window, "message", function(e) {
            try {
                if (QSI.Orchestrator && QSI.Orchestrator.isMessageEventOriginAllowed && !QSI.Orchestrator.isMessageEventOriginAllowed(e.origin)) return;
                if (e.data && "string" == typeof e.data) {
                    var t = e.data.split("|");
                    if ("QualtricsEOS" === t[0]) {
                        var i = t[1],
                            n = t[2];
                        QSI.history.logSurvey(i, n)
                    }
                }
            } catch (e) {
                "undefined" != typeof QSI && QSI.dbg && QSI.dbg.e && QSI.dbg.e(e)
            }
        });
        for (var i = e ? [e] : Object.keys(QSI.Request), n = 0; n < i.length; n++) {
            for (var r in QSI.Request[i[n]].Intercepts)
                if (Object.prototype.hasOwnProperty.call(QSI.Request[i[n]].Intercepts, r)) {
                    var a = QSI.Request[i[n]].Intercepts[r];
                    if (!QSI.reg[r]) {
                        var o;
                        if (QSI.history.logIntercept(r, a.Targeting.Decision.ActionSetID), a.siid = i[n], a.Intercept) {
                            var s = a.Intercept.DisplayOptions;
                            o = void 0 !== s.displayInterceptType ? s.displayInterceptType : !0 === s.manualDisplay ? "manual" : "onload"
                        }!t || QSI.Request[i[n]].zoneManualDisplay && "manual" !== o ? QSI.Orchestrator.setInterceptDisplayOptionCallback(o, QSI.RunIntercept.bind(null, r, a)) : "manual" === o && QSI.RunIntercept(r, a)
                    }
                } QSI.isDebug && !QSI.debuggerHasDisplayed && (new QSI.DebugHandler(QSI.Request[i[n]].Debug), QSI.debuggerHasDisplayed = !0)
        }
        var c = document.createEvent("Event");
        c.initEvent("Resolved", !0, !0), document.body.dispatchEvent(c)
    }, QSI.Orchestrator.addPopUnderTarget = function(e, t) {
        t.Options.width = t.Options.baseElement.style.width, t.Options.height = t.Options.baseElement.style.height, t.Options.helperScriptSrc = QSI.baseURL + "Orchestrator.php";
        var i = {
            onPopForwardShowTarget: t.Options.showTargetOnPopForward,
            showOnPageChange: t.Options.showOnPageChange,
            showOnSiteExit: t.Options.showOnSiteExit,
            checkThreshold: 3,
            watchInterval: 1e3,
            targetWidth: e.actionSet.ActionOptions.targetWidth,
            targetHeight: e.actionSet.ActionOptions.targetHeight,
            targetFullScreen: e.actionSet.ActionOptions.targetFullScreen,
            impressionURL: QSI.CreativeManager.Utilities.getImpressionURL(e)
        };
        e.actionSet.ActionOptions.targetPopUnderDisplay.creative = {
            elements: t.Options.elements,
            resetStyle: QSI.CreativeManager.Utilities.getResetStyle("QSIPopUnder"),
            popunderOptions: t.Options,
            popunderHelperOptions: i
        }
    }, QSI.RunIntercept = function(e, t) {
        try {
            if (void 0 === t.Targeting.Decision.ActionSetID || null === t.Targeting.Decision.ActionSetID) return;
            var i = t.Intercept.ActionSets[t.Targeting.Decision.ActionSetID];
            Q = i.EmbeddedData, (!QSI.ed[e] || Q && 0 !== Q.length) && (QSI.ed[e] = Q), QSI.global.intercepts[e] = {
                CreativeID: t.Targeting.Decision.Creative.ID,
                ASID: t.Targeting.Decision.ActionSetID
            };
            var n = {
                    id: t.siid,
                    interceptID: t.Targeting.InterceptID,
                    intercept: t.Intercept,
                    actionSet: t.Intercept.ActionSets[t.Targeting.Decision.ActionSetID],
                    creative: t.Creative,
                    decision: t.Targeting.Decision,
                    params: QSI.Request[t.siid].Params
                },
                r = t.Targeting.Decision.Creative.LocatorValues;
            if (r)
                for (var a in r)
                    if (Object.prototype.hasOwnProperty.call(r, a)) {
                        var o = r[a];
                        if (t.Creative.Options && t.Creative.Options.content && (t.Creative.Options.content = QSI.Orchestrator.replaceAll(t.Creative.Options.content, a, o)), t.Creative.Options && t.Creative.Options.html && (t.Creative.Options.html = QSI.Orchestrator.replaceAll(t.Creative.Options.html, a, o)), t.Creative.Options && t.Creative.Options.elements)
                            for (var s in t.Creative.Options.elements.Elements)
                                if (Object.prototype.hasOwnProperty.call(t.Creative.Options.elements.Elements, s)) {
                                    var c = t.Creative.Options.elements.Elements[s];
                                    c.content = QSI.Orchestrator.replaceAll(c.content, a, o)
                                }
                    } var l = t.Targeting.Decision.Creative.AnchorTags;
            if (l)
                for (var p in l)
                    if (Object.prototype.hasOwnProperty.call(l, p)) {
                        var d = l[p];
                        if (t.Creative.Options && t.Creative.Options.content && (t.Creative.Options.content = t.Creative.Options.content.replace(new RegExp(p, "i"), d)), t.Creative.Options && t.Creative.Options.elements) {
                            var I = t.Creative.Options.elements.Elements;
                            if (I && I.length)
                                for (var u = 0; u < I.length; u++) I[u].content = I[u].content.replace(new RegExp(p, "i"), d)
                        }
                    } n.actionSet.ActionOptions.targetPopUnderDisplay && t.PopUnderTarget && ("Target" === t.Targeting.Decision.PopUnderTarget.ID ? n.actionSet.ActionOptions.targetPopUnderDisplay.id = "Target" : QSI.Orchestrator.addPopUnderTarget(n, t.PopUnderTarget));

            function S() {
                t.Creative ? QSI.CreativeManager.isCreativeSupported(t.Creative.Type) ? QSI.CreativeManager["run" + t.Creative.Type](n) : QSI.dbg.c("Creative type '" + t.Creative.Type + "' is not supported.") : QSI.CreativeManager.runNoCreative(n)
            }
            var g;
            if (i.ActionOptions.useCustomTrigger) "scroll" === i.ActionOptions.triggerType ? (g = function() {
                try {
                    var e = i.ActionOptions.scrollPercentage ? i.ActionOptions.scrollPercentage : 0;
                    QSI.util.hasReachedScrollPosition(e) && (S(), QSI.util.stopObserving(window, i.ActionOptions.triggerType, g))
                } catch (e) {
                    QSI.dbg.e(e)
                }
            }, QSI.util.observe(window, i.ActionOptions.triggerType, g)) : i.ActionOptions.triggerEntirePage ? (g = function() {
                try {
                    S(), QSI.util.stopObserving(document.body, i.ActionOptions.triggerType, g)
                } catch (e) {
                    QSI.dbg.e(e)
                }
            }, QSI.util.observe(document.body, i.ActionOptions.triggerType, g)) : "pageLoad" === i.ActionOptions.triggerType ? S() : (g = function() {
                try {
                    S(), QSI.util.stopObserving(QSI.util.$(i.ActionOptions.triggerElementID), i.ActionOptions.triggerType, g)
                } catch (e) {
                    QSI.dbg.e(e)
                }
            }, QSI.util.observe(QSI.util.$(i.ActionOptions.triggerElementID), i.ActionOptions.triggerType, g));
            else S()
        } catch (e) {
            QSI.dbg.e(e)
        }
        var Q
    }, QSI.Orchestrator.init()
} catch (e) {
    QSI.dbg.e(e)
}
